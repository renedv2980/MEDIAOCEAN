*          DATA SET CTSFM55    AT LEVEL 166 AS OF 05/01/02                      
*PHASE TA0A55A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM55 --PROGRAM ASSIGNMENT MAINTENANCE/LIST/REPORT *         
*                                                                     *         
*  COMMENTS:     MAINTAINS PROGRAM ASSIGN. RECORDS ON GENDIR/GENFIL   *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMA5 (MAINTENANCE)                        *         
*                        CTSFMB5 (LIST)                               *         
*                        CTSFMC5 (REPORT)                             *         
*                                                                     *         
*  OUTPUTS:      UPDATED ASSIGNMENT RECORDS, LIST, OR REPORT.         *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOLD                                         *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A55 - PROGRAM ASSIGNMENT RECORD MAINT/LIST/REPORT'           
TA0A55   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A55**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING TA0AFFD,RA          BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
*                                                                               
VK       XC    FILTER,FILTER       FILTER - STAFF ID                            
         XC    FILTCK,FILTCK       FILTER CODE                                  
         XC    PERSON,PERSON       PERSON                                       
         XC    SEQU,SEQU           PERSON SEQUENCE CODE                         
         XC    PROGRAM,PROGRAM     PROGRAM                                      
         XC    SYST,SYST           SYSTEM                                       
         LA    R2,APMSYSTH                                                      
         CLI   APMSYSTH+5,0        SYSTEM ENTERED?                              
         BE    VK30                NO                                           
         CLI   ACTEQU,ACTREP                                                    
         BNE   VK05                                                             
         GOTO1 ANY                                                              
         CLC   =C'ALL     ',WORK   'ALL' = REPORT ALL SYSTEMS                   
         BE    VK60                                                             
*                                                                               
*                                  YES THEN VALIDATE SYSTEM                     
VK05     LA    R2,APMSYSTH         SYSTEM NAME                                  
         GOTO1 ANY                 MOVES NAME INTO WORK                         
         GOTO1 GETFACT,DMCB,0      GET A(FACTSD)                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         L     R5,FASYSLST         A(SYSTEM LIST)                               
         DROP  R1                                                               
*                                                                               
         USING SYSLSTD,R5                                                       
         LH    R6,0(R5)            LENGTH OF TABLE ENTRY                        
         L     R7,2(R5)            A(END OF TABLE)                              
         LA    R5,6(R5)            A(FIRST ENTRY)                               
         ZIC   R3,5(R2)            INPUT LENGTH                                 
         BCTR  R3,0                                                             
*                                                                               
VK10     EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SYSLNAME    MATCH ON SYSTEM NAME?                        
         BE    VK15                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SYSLSHRT    MATCH ON SHORT SYSTEM NAME?                  
         BE    VK15                                                             
         BXLE  R5,R6,VK10          TRY NEXT TABLE ENTRY                         
         MVC   GERROR,=AL2(INVSYS) SYSTEM NAME NOT IN TABLE                     
         B     VSFMERR                                                          
*                                                                               
VK15     MVC   SYST,SYSLNAME       KEEP LONG SYSTEM NAME                        
         DROP  R5                                                               
*                                                                               
         CLI   ACTEQU,ACTLIST      ON-SCREEN LIST ACTION?                       
         BE    VK45                WHEN SYSTEM IS SPECIFIED                     
         CLI   ACTEQU,ACTREP       REPORT ACTION?                               
         BE    VK60                WHEN SYSTEM IS SPECIFIED                     
VK20     XC    PROGRAM,PROGRAM     PROGRAM NAME                                 
         LA    R2,APMPROGH                                                      
         CLI   APMPROGH+5,0        ENTERED PROG?                                
         BE    VK60                                                             
         GOTO1 ANY                 TO PUT PROGRAM INTO WORK                     
         CLC   =C'ALL     ',WORK   'ALL' = SYSTEM MANAGER                       
         BE    VK60                NO PROGRAM FOR 'ALL'                         
         ZIC   R5,APMPROGH+5       LENGTH ENTERED                               
         BCTR  R5,0                MINUS 1 FOR EX                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   PROGRAM(0),WORK     SAVE PROGRAM NAME IF NOT ALL                 
         B     VK60                                                             
VK30     CLI   ACTEQU,ACTREP       REPORT ACTION?                               
         BE    VK50                                                             
         CLI   ACTEQU,ACTLIST      ON-SCREEN LIST ACTION?                       
         BE    VK40                WITH NO SYSTEM SPECIFIED                     
         GOTO1 ANY                 WILL GIVE USER MISSING ERROR                 
*                                                                               
VK40     XC    SYST,SYST                                                        
         LA    R2,APLFILTH                                                      
         GOTO1 ANY                 STAFF FILTER REQUIRED IF NO SYSTEM           
         B     *+12                                                             
VK45     CLI   APLFILTH+5,0        ANY FILTER? (WITH SYSTEM)                    
         BE    VK20                NO                                           
*                                  YES                                          
         MVI   FILTCK,1            1= YES THERE IS A FILTER                     
         LA    R2,APLFILTH                                                      
         GOTO1 ANY                 TO PUT FILTER INTO WORK                      
         CLI   APLFILTH+5,8        ENTERED FULL ID?                             
         BNE   *+14                                                             
         MVC   FILTER,WORK                                                      
         B     VK48                                                             
         CLI   APLFILTH+5,5        ENTERED ABBR. ID?                            
         BE    *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         MVC   FILTER(4),WORK                                                   
         MVC   FILTER+4(4),=C'DDNY'                                             
VK48     MVC   CKSTAFF,FILTER                                                   
         BAS   RE,VSTAFF                                                        
         B     VK60                                                             
*                                                                               
VK50     LA    R2,APRPERSH                                                      
         CLI   APRSYSTH+5,0        PERSON SPECIFIED?                            
         BE    VK55                NO                                           
         GOTO1 ANY                 TO PUT PERSON INTO WORK                      
         CLI   APRPERSH+5,8        ENTERED FULL ID?                             
         BNE   *+14                                                             
         MVC   PERSON,WORK                                                      
         B     VK53                                                             
         CLI   APRPERSH+5,5        ENTERED ABBR. ID?                            
         BE    *+14                                                             
         MVC   GERROR,=AL2(INVALID)                                             
         B     VSFMERR                                                          
         MVC   PERSON(4),WORK                                                   
         MVC   PERSON+4(4),=C'DDNY'                                             
VK53     MVC   CKSTAFF,PERSON                                                   
         BAS   RE,VSTAFF                                                        
         B     VK60                                                             
*                                                                               
VK55     LA    R2,APRSEQUH         PERSON SEQUENCE                              
         CLI   APRSEQU,C'Y'       REQUESTED?                                    
         BE    *+18                                                             
         LA    R2,APRSYSTH                                                      
         MVC   GERROR,=AL2(MISSING)                                             
         B     VSFMERR                                                          
         MVI   SEQU,1               TO INDICATE PERSON SEQUENCE REQUEST         
         B     VK60                                                             
*                                                                               
*                                                                               
VK60     LA    R4,KEY              BUILD KEY                                    
         USING APASKEYD,R4                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   APASSYS,APASSYSQ    SYSTEM                                       
         MVI   APASTYP,APASTYPQ    RECORD TYPE                                  
         MVC   APASSYM,SYST        SYSTEM                                       
         MVC   APASPGM,PROGRAM     PROGRAM                                      
         MVC   KEY2,KEY                                                         
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       L     R4,AIO              BUILD RECORD                                 
         USING APASKEY,R4                                                       
         MVC   SYST,APASSYM        SYSTEM                                       
         MVC   PROGRAM,APASPGM     PROGRAM                                      
*                                                                               
         MVI   ELCODE,PRDSCELQ     DESCRIPTION ELEMENT                          
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING PRDSCD,R6                                                        
         XC    ELEM,ELEM                                                        
         MVI   PRDSCEL,PRDSCELQ                                                 
         LA    R2,APMDESCH                                                      
         GOTO1 ANY                                                              
         ZIC   R3,APMDESCH+5       LENGTH OF DESCRIPTION                        
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   PRDSC(0),WORK       ACTUAL DESCRIPTION                           
         A     R3,=F'3'            ADD FIXED AND 1 FOR EX                       
         STC   R3,PRDSCLN                                                       
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,STAFFELQ     STAFF INFO ELEMENT                           
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING STAFFD,R6                                                        
         XC    ELEM,ELEM                                                        
         LA    R4,APMIDTGH         A(END OF ID FIELDS)                          
         ST    R4,ENDID                                                         
         LA    R2,APMPGIDH         FIRST ID FIELD HEADER                        
         LA    R5,1                DEPARTMENT CODE                              
         STC   R5,DEPTCD                                                        
VR50     CLI   5(R2),0                                                          
         BNE   VR55                                                             
VR53     LA    R5,1(R5)            NEXT DEPARTMENT CODE                         
         STC   R5,DEPTCD                                                        
         ZIC   R3,0(R2)            BUMP TO NEXT FIELD (NAME)                    
         AR    R2,R3                                                            
         LA    R2,64(R2)           BUMP PAST NAME/TEL FIELDS                    
         ZIC   R3,0(R2)                                                         
         AR    R2,R3               BUMP TO NEXT ID FIELD                        
         C     R2,ENDID            PAST ALL ID FIELDS?                          
         BNL   VR5X                                                             
         B     VR50                                                             
VR55     MVI   STAFFEL,STAFFELQ                                                 
         MVI   STAFFLN,STAFFLNQ                                                 
         MVC   STAFFCD,DEPTCD                                                   
*                                                                               
         CLI   5(R2),5                                                          
         BNE   *+20                                                             
         MVC   STAFFID(4),8(R2)                                                 
         MVC   STAFFID+4(4),=C'DDNY'                                            
         B     *+10                                                             
         MVC   STAFFID,8(R2)                                                    
         MVC   CKSTAFF,8(R2)                                                    
         BAS   RE,VSTAFF              VALIDATE STAFF ID                         
         GOTO1 ADDELEM                                                          
         XC    ELEM,ELEM                                                        
         B     VR53                                                             
VR5X     DS    0H                                                               
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,REMKSELQ     REMARKS ELEMENT                              
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING REMKSD,R6                                                        
         LA    R2,APMRMK1H         FIRST REMARK HEADER                          
         LA    R5,1                                                             
VR70     CLI   5(R2),0                                                          
         BE    VR7X                                                             
         XC    ELEM,ELEM                                                        
         MVI   REMKSEL,REMKSELQ    ELEMENT CODE                                 
         ZIC   R3,5(R2)            LENGTH OF REMARK                             
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   REMKSTXT(0),8(R2)   ACTUAL REMARK                                
         LA    R3,1(R3)                                                         
         A     R3,=F'3'            LENGTH BEFORE VARIABLE                       
         STC   R3,REMKSLN                                                       
         STC   R5,REMKSCD          LINE NUMBER                                  
         GOTO1 ADDELEM                                                          
         LA    R2,78(R2)           NEXT HEADER                                  
         LA    R5,1(R5)            INCREMENT LINE COUNTER                       
         C     R5,=F'4'            ONLY 4 REMARK LINES                          
         BNH   VR70                                                             
VR7X     DS    0H                                                               
         DROP  R6                                                               
*                                                                               
         B     DR                  DISPLAY THE RECORD                           
         DROP  R4                                                               
         EJECT                                                                  
*VALIDATE STAFF ID WITH STAFF RECORDS                                           
*                                                                               
VSTAFF   NTR1                                                                   
         MVC   AIO,AIO2            DON'T MESS UP CURRENT AIO                    
         XC    NAME,NAME                                                        
         XC    EXTEN,EXTEN                                                      
         XC    HOMEPH,HOMEPH                                                    
         LA    R4,NEWKEY           BUILD STAFF RECORD KEY                       
         USING ASTFKEYD,R4                                                      
         XC    NEWKEY,NEWKEY                                                    
         MVI   ASTFSYS,ASTFSYSQ                                                 
         MVI   ASTFTYP,ASTFTYPQ                                                 
         MVC   ASTFPRID,CKSTAFF                                                 
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'GENDIR',NEWKEY,NEWKEY                 
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL',NEWKEY+36,AIO2,DMWORK         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*        *                                                                      
         L     R6,AIO2                                                          
         MVI   ELCODE,STNAMELQ     NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING STNAMD,R6                                                        
         MVC   NAME,STNAME         STAFF MEMBER'S NAME                          
         DROP  R6                                                               
*        *                                                                      
         L     R6,AIO2                                                          
         MVI   ELCODE,PHNUMELQ     PHONE NUMBERS ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING PHNUMD,R6                                                        
         MVC   EXTEN,PHNUMOFF      OFFICE EXTENTION                             
         MVC   HOMEPH,PHNUMHME     HOME PHONE NUMBER                            
         DROP  R4                                                               
         DROP  R6                                                               
         MVC   AIO,AIO1            RETURN TO AIO1                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R0,APMTAGH          LAST FIELD ON SCREEN                         
         LA    R2,APMDESCH         FIRST DATA FIELD HEADER                      
*                                                                               
DR10     ZIC   R1,0(R2)            LENGTH OF FIELD + HEADER                     
         SH    R1,=H'17'           MINUS HEADER, EXTEN, AND 1 FOR EX            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT FIELD                              
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DR20     ZIC   R1,0(R2)            RESTORE LENGTH                               
         AR    R2,R1               NEXT SCREEN FIELD                            
         CR    R2,R0               END OF SCREEN?                               
         BE    *+16                                                             
         TM    1(R2),X'20'         NO -- FIELD IS PROTECTED?                    
         BZ    DR10                NO -- CLEAR IT                               
         B     DR20                YES -- BUMP TO NEXT FIELD                    
*                                                                               
         MVC   APMPGNM,SPACES      CLEAR PROTECTED DATA FIELDS                  
         OI    APMPGNMH+6,X'80'                                                 
         MVC   APMPGPH,SPACES                                                   
         OI    APMPGPHH+6,X'80'                                                 
         MVC   APMPGPO,SPACES                                                   
         OI    APMPGPOH+6,X'80'                                                 
         MVC   APMTPNM,SPACES                                                   
         OI    APMTPNMH+6,X'80'                                                 
         MVC   APMTPPH,SPACES                                                   
         OI    APMTPPHH+6,X'80'                                                 
         MVC   APMTPPO,SPACES                                                   
         OI    APMTPPOH+6,X'80'                                                 
         MVC   APMSVNM,SPACES                                                   
         OI    APMSVNMH+6,X'80'                                                 
         MVC   APMSVPH,SPACES                                                   
         OI    APMSVPHH+6,X'80'                                                 
         MVC   APMSVPO,SPACES                                                   
         OI    APMSVPOH+6,X'80'                                                 
         MVC   APMCLNM,SPACES                                                   
         OI    APMCLNMH+6,X'80'                                                 
         MVC   APMCLPH,SPACES                                                   
         OI    APMCLPHH+6,X'80'                                                 
         MVC   APMCLPO,SPACES                                                   
         OI    APMCLPOH+6,X'80'                                                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PRDSCELQ     DESCRIPTION                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING PRDSCD,R6                                                        
         ZIC   R3,1(R6)            ELEMENT LENGTH                               
         S     R3,=F'2'            SUBTRACT FIXED                               
         BCTR  R3,0                AND ONE MORE FOR EX                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   APMDESC(0),PRDSC                                                 
         OI    APMDESCH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         LA    R2,APMPGIDH         FIRST ID FIELD HEADER                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,STAFFELQ     STAFF INFO ELEMENTS                          
         BAS   RE,GETEL                                                         
         BNE   DR40                                                             
DR33     DS    0H                                                               
         USING STAFFD,R6                                                        
         LA    R2,APMPGIDH         FIRST ID FIELD HEADER                        
         CLI   STAFFCD,1           PROGRAMMER STAFF ID                          
         BE    DR30                                                             
         LA    R2,111(R2)          SKIP TO NEXT ID HEADER                       
         CLI   STAFFCD,2           TECH. PUB. STAFF ID                          
         BE    DR30                                                             
         LA    R2,111(R2)                                                       
         CLI   STAFFCD,3           CLIENT SERVICE STAFF ID                      
         BE    DR30                                                             
         LA    R2,111(R2)                                                       
         CLI   STAFFCD,4           CALL SUPPORT STAFF ID                        
         BE    DR30                                                             
         B     DR40                                                             
DR30     MVC   8(8,R2),STAFFID                                                  
         OI    6(R2),X'80'                                                      
         MVC   CKSTAFF,STAFFID                                                  
         BAS   RE,VSTAFF                                                        
         ZIC   R1,0(R2)            BUMP TO NAME HEADER                          
         AR    R2,R1                                                            
         MVC   8(L'NAME,R2),NAME                                                
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)            BUMP TO HOME PHONE HEADER                    
         AR    R2,R1                                                            
         MVC   8(L'HOMEPH,R2),HOMEPH                                            
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)            BUMP TO OFF. EXT. HEADER                     
         AR    R2,R1                                                            
         MVC   8(L'EXTEN,R2),EXTEN                                              
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   DR40                                                             
         B     DR33                                                             
*                                                                               
DR40     L     R6,AIO                                                           
         MVI   ELCODE,REMKSELQ     REMARKS ELEMENTS                             
         BAS   RE,GETEL                                                         
         BNE   DRX                                                              
         LA    R2,APMRMK1H                                                      
         B     *+12                                                             
DR45     BAS   RE,NEXTEL                                                        
         BNE   DRX                                                              
         USING REMKSD,R6                                                        
         ZIC   R4,REMKSLN          LENGTH OF ELEMENT                            
         S     R4,=F'3'            LENGTH OF FIXED                              
         BCTR  R4,0                ONE MORE FOR THE EX                          
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),REMKSTXT                                                 
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)            NEXT REMARK HEADER                           
         AR    R2,R1                                                            
         B     DR45                                                             
         DROP  R6                                                               
*                                                                               
DRX      MVC   KEY,KEY2            TO AVOID PUTREC DRAMA                        
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
X        B     XIT                                                              
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R4,AIO              SELECTED RECORD                              
         USING APASKEYD,R4                                                      
*                                                                               
         MVC   APMSYST,APASSYM     SYSTEM                                       
         OI    APMSYSTH+6,X'80'                                                 
         MVC   APMPROG,APASPGM     PROGRAM NAME                                 
         OI    APMPROGH+6,X'80'                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* ON-SCREEN LIST                                                                
*                                                                               
LR       LA    R4,KEY                                                           
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
*                                                                               
         MVI   APASSYS,APASSYSQ    SYSTEM                                       
         MVI   APASTYP,APASTYPQ    RECORD TYPE                                  
         MVC   APASSYM,SYST        SYSTEM                                       
         MVC   APASPGM,PROGRAM     PROGRAM                                      
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(2),SAVEKEY      SAME REC TYPE?                               
         BNE   LRX                                                              
         CLI   FILTCK,1            FILTER?                                      
         BE    *+14                                                             
         CLC   KEY(14),SAVEKEY     SAME SYSTEM?                                 
         BNE   LRX                 NOPE                                         
*                                                                               
         GOTO1 GETREC              GET THE PROGRAM ASSIGN. RECORD               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
*                                                                               
         CLI   FILTCK,1                                                         
         BNE   LR33                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,STAFFELQ     STAFF INFO ELEMENTS                          
         BAS   RE,GETEL                                                         
         BNE   LR20                NEXT RECORD                                  
LR32     DS    0H                                                               
         USING STAFFD,R6                                                        
         CLC   FILTER,STAFFID      MATCH?                                       
         BE    LR33                SHOW RECORD                                  
         BAS   RE,NEXTEL                                                        
         BNE   LR20                NEXT RECORD                                  
         B     LR32                                                             
*                                                                               
LR33     MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         USING APASKEYD,R4                                                      
         MVC   LSTSYST,APASSYM     PUT SYSTEM IN LIST LINE                      
         MVC   LSTPROG,APASPGM     PUT PROGRAM IN LIST LINE                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PRDSCELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING PRDSCD,R6                                                        
         ZIC   R2,PRDSCLN          ELEMENT LENGTH                               
         S     R2,=F'2'            MINUS FIXED                                  
         BCTR  R2,0                ONE MORE FOR EX                              
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   LSTDESC(0),PRDSC    PUT DESCRIPTION IN LIST LINE                 
*                                                                               
*                                                                               
LR50     GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* OFFLINE PRINT                                                                 
*                                                                               
PR       LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
*        ICM   RF,15,ABOX          DO WE HAVE A(BOXES)?                         
*                                                                               
PR10     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   APASSYS,APASSYSQ    SYSTEM                                       
         MVI   APASTYP,APASTYPQ    RECORD TYPE                                  
         MVC   APASSYM,SYST        SYSTEM                                       
         MVC   APASPGM,PROGRAM     PROGRAM                                      
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR20     MVC   KEYSAVE,KEY                                                      
         LA    R4,KEY              NEXT RECORD                                  
         GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    PR30                                                             
         DC    H'0'                                                             
*                                                                               
PR30     CLC   KEY(2),SAVEKEY      APASS RECORD?                                
         BNE   PRX                 NO MORE TO REPORT                            
*                                                                               
         OC    SYST,SYST           ANY SYSTEM FILTER?                           
         BZ    *+14                                                             
         CLC   SYST,APASSYM        YES - MATCH ON SYSTEM                        
         BNE   PRX                 NO                                           
*                                                                               
         CLC   APASSYM,(APASSYM-APASKEY)+KEYSAVE                                
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       EJECT PAGE ON SYSTEM/PROGRAM CHANGE          
*                                                                               
         GOTO1 GETREC              GET THE SCROLLER DATATYPE RECORD             
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
*                                                                               
         MVC   PRTSYST,APASSYM     PUT SYSTEM NAME IN PRINT LINE                
         MVC   PRTPROG,APASPGM     PUT PROGRAM IN PRINT LIN                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PRDSCELQ     DESCRIPTION ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
         USING PRDSCD,R6                                                        
         ZIC   R3,1(R6)            ELEMENT LENGTH                               
         S     R3,=F'2'            SUBTRACT FIXED                               
         BCTR  R3,0                AND ONE MORE FOR EX                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   PRTDESC(0),PRDSC    PUT DESCRIPTION IN PRINT LINE                
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,STAFFELQ     STAFF INFO ELEMENTS                          
         BAS   RE,GETEL                                                         
         BNE   PR33                                                             
PR31     DS    0H                                                               
         USING STAFFD,R6                                                        
         CLI   STAFFCD,1           PROGRAMMER STAFF ID                          
         BNE   *+10                                                             
         MVC   PRTPGM,STAFFID                                                   
         CLI   STAFFCD,2           TECH. PUB. STAFF ID                          
         BNE   *+10                                                             
         MVC   PRTDOC,STAFFID                                                   
         CLI   STAFFCD,3           CLIENT SERVICE STAFF ID                      
         BNE   *+10                                                             
         MVC   PRTSER,STAFFID                                                   
         CLI   STAFFCD,4           CALL SUPPORT STAFF ID                        
         BNE   *+10                                                             
         MVC   PRTCAL,STAFFID                                                   
         BAS   RE,NEXTEL                                                        
         BNE   PR33                                                             
         B     PR31                                                             
*                                                                               
PR33     L     R6,AIO                                                           
         MVI   ELCODE,REMKSELQ     REMARKS ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   PR40                                                             
         USING REMKSD,R6                                                        
         ZIC   R5,REMKSLN          LENGTH OF ELEMENT                            
         S     R5,=F'3'            LENGTH OF FIXED                              
         BCTR  R5,0                ONE MORE FOR THE EX                          
         LA    R1,14               CHECK TO SEE IF TOO LONG TO FIT              
         CR    R5,R1                                                            
         BL    *+8                                                              
         LA    R5,14               MAX LENGTH                                   
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   PRTREM(0),REMKSTXT                                               
         DROP  R6                                                               
*                                                                               
*                                                                               
PR40     DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT RECORD                                 
         B     PR20                NEXT RECORD                                  
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
* HEAD HOOK ROUTINE                                                             
*                                                                               
HDHOOK   NTR1                                                                   
*                                                                               
         MVC   H1+50(25),=C'PROGRAM ASSIGNMENT REPORT'                          
         MVI   H2+50,X'BF'         UNDERLINE CHARACTER                          
         MVC   H2+51(24),H2+50                                                  
*        CLI   SEQU,1               PERSON SEQUENCE                             
*        BE    *+10                YES                                          
         MVC   H4+8(L'APASSYM),APASSYM    SYSTEM                                
*        MVC   H4+8(8),STAFFID                                                  
*                                                                               
         ICM   RF,15,ABOX          DO WE HAVE A(BOXES)?                         
         BZ    HDHOOKX             NO                                           
*                                                                               
         USING BOXD,RF             DEFINE BOX AREA                              
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+5,C'T'                                                   
         MVI   BOXROWS+7,C'M'                                                   
         MVI   BOXROWS+57,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+8,C'C'                                                   
         MVI   BOXCOLS+17,C'C'                                                  
         MVI   BOXCOLS+75,C'C'                                                  
         MVI   BOXCOLS+84,C'C'                                                  
         MVI   BOXCOLS+93,C'C'                                                  
         MVI   BOXCOLS+102,C'C'                                                 
         MVI   BOXCOLS+111,C'C'                                                 
         MVI   BOXCOLS+127,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  RF                                                               
*                                                                               
HDHOOKX  B     XIT                                                              
         SPACE 3                                                                
HEDSPECS SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H4,1,C'SYS/PER'                                                  
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H7,2,C'SYSTEM  PROGRAM  DESCRIPTION'                             
         SSPEC H7,77,C'PROGMER  DOCUMER  CLT.SER. CALL SPT REMARKS'             
         DC    X'00'                                                            
         EJECT                                                                  
VSFMERR  GOTO1 SFMERR                                                           
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
RELO     DS    F                                                                
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
* DDBIGBOX                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FASYSLST                                                                      
* FASYSLSTD                                                                     
* CTSFMFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
       ++INCLUDE FASYSFAC                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMA5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMB5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
*      ++INCLUDE CTSFMC5D                                                       
         EJECT                                                                  
       ++INCLUDE CTGENAPASS                                                     
         EJECT                                                                  
       ++INCLUDE CTGENASTAF                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
         DS    0F                                                               
ENDID    DS    XL4                                                              
FILTER   DS    CL8                                                              
PROGRAM  DS    CL8                                                              
CKSTAFF  DS    CL8                                                              
PERSON   DS    CL8                                                              
NAME     DS    CL24                                                             
EXTEN    DS    CL4                                                              
HOMEPH   DS    CL12                                                             
KEY2     DS    XL32                                                             
SAVEKEY  DS    XL32                                                             
NEWKEY   DS    XL40                                                             
SYST     DS    CL7                                                              
RMKLINE  DS    XL1                                                              
DEPTCD   DS    XL1                                                              
FILTCK   DS    XL1                                                              
SEQU     DS    XL1                                                              
         SPACE 5                                                                
* ON-SCREEN LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL3                                                              
LSTSYST  DS    CL8                 SYSTEM NAME                                  
         DS    CL2                                                              
LSTPROG  DS    CL8                 PROGRAM NAME                                 
         DS    CL10                                                             
LSTDESC  DS    CL57                DESCRIPTION                                  
         SPACE 5                                                                
* PRINT LINE                                                                    
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    C                   LEFT BOX MARGIN                              
PRTSYST  DS    CL7                 DTYPE NAME                                   
         DS    C                                                                
PRTPROG  DS    CL8                 PROGRAM                                      
         DS    C                                                                
PRTDESC  DS    CL57                DESCRIPTION                                  
         DS    C                                                                
PRTPGM   DS    CL8                 PROGRAMMER                                   
         DS    C                                                                
PRTDOC   DS    CL8                 TECHNICAL WRITER                             
         DS    C                                                                
PRTSER   DS    CL8                 CLIENT SERVICE                               
         DS    C                                                                
PRTCAL   DS    CL8                 CALL SUPPORT                                 
         DS    C                                                                
PRTREM   DS    CL15                FIRST REMARK LINE TRUNCATED                  
         DS    C                   RIGHT BOX MARGIN                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'166CTSFM55   05/01/02'                                      
         END                                                                    
