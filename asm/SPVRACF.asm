*          DATA SET SPVRACF    AT LEVEL 006 AS OF 02/22/18                      
*PROCESS USING(WARN(15))                                                        
*PHASE PVRACFPA                                                                 
*>>>>>>> LIVE PHASE NAME MUST BE "PVRACF". SEE NOTE BELOW <<<<<<                
*SETOPT PARM(REUS=RENT)                                                         
*                                                                               
*&&      SET   DEBUG=N                                                          
*                                                                               
*>>>>>>>>>>>>>>>>>>>>>>>>>>>  READ THIS!   <<<<<<<<<<<<<<<<<<<<<<<<<<<<         
*                                                                               
**** AFTER PROMOTION TO PROD VIA PANAPT, THE "PVRACFP" PRODUCTION               
**** LOAD MODULE MUST BE COPIED MANUALLY TO THE CURRENT PANVALET                
**** PRODUCTION LOAD LIBRARY, AND RENAMED TO "PVRACF". PVRACF IS THE            
**** MEMBER NAME SPECIFIED ON THE EXITDEF MACRO IN PVEXTUSR.                    
**** AS OF AUG/2017, THE PANVALET LOAD LIBRARY IS 'SYS2.CA.PANV.LOAD'           
*                                                                               
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<         
*                                                                               
         TITLE '*** PVRACF: PANVALET RACF SECURITY EXIT ***'                    
*                                                                               
* NOTE: THIS IS THE PANVALET RACF SECURITY EXIT AS DELIVERED BY CA, BUT         
*       WITH THE FOLLOWING CUSTOM MODIFICATIONS:                                
*                                                                               
*   1. CODE PERTAINING TO OLD RACF VERSIONS (PRE 1.9) HAS BEEN REMOVED.         
*                                                                               
*   2. CODE CONFORMS TO DDS STANDARDS WHEREVER POSSIBLE.                        
*                                                                               
*   3. IF AN APPLICABLE RACF PSEUDO-PROFILE IS *NOT* FOUND, ACCESS              
*       IS *GRANTED*. (IN THE CA-DELIVERED EXIT, ACCESS IS *DENIED*             
*       IF NO PSEUDO-PROFILE IS FOUND.) PROVIDED THAT WE HAVE                   
*       THE CORRECT RACF PSEUDO-PROFILES IN PLACE FOR THE *PRODUCTION*          
*       PANVALET LIBRARIES (I.E., THOSE THAT REQUIRE SPECIFIC                   
*       PROTECTION), IT'S OKAY TO ALLOW ACCESS WITHOUT A PROFILE.               
*                                                                               
*       NOTE: THE RULE ABOVE APPLIES ONLY TO THE "ORIGINAL" PSEUDO-             
*       PROFILES THAT WE'VE BEEN USING FOREVER. THE NEW                         
*       LPAR-SPECIFIC PSEUDO-PROFILES (DESCRIBED BELOW) MUST BE                 
*       DEFINED FOR EVERY RESOURCE.                                             
*                                                                               
*   4. WE SUPPRESS VIOLATION MESSAGES ON A ++SCAN (AKA PANSCAN) ACTION.         
*       NOTE: NO ADDITIONAL AUTHORIZATION IS GRANTED, AND NO PROTECTED          
*       MEMBER IS SCANNED. BUT WE DO *NOT* LOG THE VIOLATION RESULTING          
*       FROM THE ATTEMPT TO ++SCAN ANTHING, BECAUSE IF WE DID, WE WOULD         
*       GET BEZILLIONS OF POINTLESS VIOLATIONS RESULTING FROM                   
*       ACCEPTABLE SCANS OF AN ENTIRE PRODUCTION PAN LIBRARY.                   
*                                                                               
*   5. WE ABORT PROCESSING IF RACF IS UNAVAILABLE (THIS SHOULD NEVER            
*       HAPPEN).                                                                
*                                                                               
*   6. WE BUILD AN LPAR-SPECIFIC PSEUDO-PROFILE NAME, AND INVOKE                
*       RACROUTE TO VERIFY ACCESS. THIS PREVENTS ACCESS TO MEMBERS OF           
*       A *NON-PRODUCTION* PAN LIBRARY ON A *PRODUCTION* LPAR.                  
*                                                                               
*                                                                               
* PROCEDURE/FUNCTION                                                            
* ------------------                                                            
*                                                                               
* RACFEXIT <==> SAMPLE PANVALET/RACF SECURITY EVENT EXIT                        
*                                                                               
*               THIS EXIT IS A SAMPLE OF HOW THE PANVALET GEM                   
*               FACILITY CAN BE INTERFACED WITH RACF TO PROVIDE                 
*               SECURITY FOR PANVALET LIBRARIES. THIS EXIT MAKES                
*               THE FOLLOWING ASSUMPTIONS:                                      
*                                                                               
*               1. ACCESS RULES ARE SET UP TO ALLOW ALL ACCESS TO THE           
*                  PANVALET LIBRARIES. THIS IS NECESSARY BECAUSE THE            
*                  LIBRARIES ARE OPENED FOR UPDATE.                             
*               2. THE ACTUAL ACCESS RULES ARE WRITTEN FOR DATASETS             
*                  THAT HAVE A HIGH LEVEL QUALIFIER OF PV. THE EXIT             
*                  WILL TAKE THE DATASET NAME, AND ADD A 'PV.' AT THE           
*                  FRONT OF IT. ADDITIONALLY, IF MEMBER LEVEL SUPPORT           
*                  IS DESIRED, THE MEMBER NAME IS APPENDED AT THE END           
*                  OF THE DATASET NAME. THUS, FOR LONG DATASET NAMES,           
*                  UP TO 13 CHARACTERS OF THE DSN WILL BE LOST.                 
*                  EX: THE PV DATASET IS 'PAN.APPL.LIBRARY'                     
*                      MEMBER NAME IS SPREPB102                                 
*                      THE RESULTING PROFILE NAME PASSED TO RACF IS:            
*                      PV.PAN.APPL.LIBRARY.SPREPB102                            
*               3. ADDITIONALLY, LPAR-SPECIFIC PROFILES ARE CHECKED IF          
*                  WE ARE RUNNING ON A PRODUCTION LPAR. SEE THE                 
*                  COMMENTS BELOW FOR DETAILS.                                  
*                                                                               
* VERY IMPORTANT!!! (FEB/2018):                                                 
*   BECAUSE PANVALET MEMBER NAME LENGTHS CAN EXCEED THE MAXIMUM LENGTH          
*   OF A DATASET QUALIFER NAME (VIZ.: 8), THIS MEANS THAT BY CALLING            
*   RACROUTE WITH CLASS=DATASET, WE ARE POTENTIALLY CONSTRUCTING A              
*   PSEUDO-PROFILE NAME WHICH DOESN'T CONFORM TO A VALID DSN. THAT'S            
*   OKAY **IF AND ONLY IF** THE MEMBERNAME IS THE ***LAST*** QUALIFIER          
*   IN THE PSEUDO-PROFILE. THAT IS INDEED THE CASE FOR THE *ORIGINAL*           
*   PSEUDO-PROFILES, SO WE ARE OKAY THERE.                                      
*                                                                               
*   THE LPAR-SPECIFIC PSEUDO-PROFILES ARE ANOTHER MATTER, BECAUSE THE           
*   *LPAR* IS THE LAST QUALIFIER. THEREFORE, WE MUST TRUNCATE THE PAN           
*   MEMBER NAME AT EIGHT CHARACTERS. THERE'S REALLY NOT MUCH DOWNSIDE           
*   TO THAT. IT JUST MEANS THAT WE CAN'T CONSTRUCT LPAR-SPECIFIC                
*   PSEUDO-PROFILES FOR *SPECIFIC* PAN MEMBERS WITH A NAME LONGER THAN          
*   EIGHT CHARACTERS. WE CAN, HOWEVER, USE MEMBER NAMING *PREFIX*               
*   CONVENTIONS TO PROVIDE SUCH PROTECTION, AND THAT SEEMS QUITE                
*   ADEQUATE.                                                                   
*                                                                               
*                                                                               
         EJECT                                                                  
* PARAMETERS                                                                    
* ----------                                                                    
*                                                                               
*   NAME       TYPE     USAGE              DESCRIPTION                          
* --------  ----------  -----  ----------------------------------------         
*                                                                               
* EVENTBLK  STRUCTURE    IN    EVENT BLOCK                                      
*                                                                               
* ENVIRBLK  STRUCTURE    IN    ENVIRONMENT BLOCK                                
*                                                                               
* DATALIST  STRUCTURE    MOD   DATA LIST BLOCK                                  
*                                                                               
* RETURN CODE                                                                   
* -----------                                                                   
*                                                                               
* VALUE                           DESCRIPTION                                   
* -----  --------------------------------------------------------------         
*                                                                               
*   0    EXIT FUNCTION COMPLETED (STATUS RETURNED IN EVENTBLK)                  
*                                                                               
* CAUSE/EFFECT RELATIONSHIPS                                                    
* --------------------------                                                    
*                                                                               
*   NONE                                                                        
*                                                                               
* EXTERNAL EFFECTS                                                              
* ----------------                                                              
*                                                                               
*   NONE                                                                        
*                                                                               
* IMPLEMENTATION DETAILS                                                        
* ----------------------                                                        
*                                                                               
*   EXIT IS REENTRANT                                                           
*                                                                               
*   TO ASSEMBLE THE EXIT, THE DATASET SYS1.MACLIB MUST BE IN THE                
*   SYSLIB CONCATENATION.                                                       
*                                                                               
*                                                                               
*   SAMPLE JCL TO ASSEMBLE THIS EXIT IS ON THE PANVALET INSTALLATION            
*   LIBRARY, MEMBER NAME IS PVRACFJCL.                                          
*                                                                               
*                                                                               
         EJECT                                                                  
RACFEXIT RSECT                                                                  
RACFEXIT AMODE 24                  PER CA DOCUMENTATION                         
RACFEXIT RMODE 24                  PER CA DOCUMENTATION                         
*                                                                               
         PRINT NOGEN                                                            
         REQUS                                                                  
*                                                                               
         SAVE  (14,12),T,*         SAVE CALLER'S REGISTERS                      
*                                                                               
         LR    RB,RF               RB = PROGRAM BASE REGISTER                   
         USING RACFEXIT,RB                                                      
*                                                                               
         USING EXITPLST,R1         R1 = PARAMETER LIST                          
         L     R2,EXEVENT                                                       
         USING EVENTBLK,R2                                                      
         L     R6,EXENVIR                                                       
         USING ENVIRBLK,R6                                                      
         L     R4,EXDATA                                                        
         USING DATALIST,R4                                                      
         DROP  R1                                                               
*                                                                               
         USING DLENTRY,R5          DATA LIST ENTRY BASE REGISTER                
*                                                                               
         IF  CLC,EVEVENT,NE,=CL8'INIT'  PANVALET EXIT INIT EVENT?               
           L    R7,EVUSER          NO: WE HAVE A(EXIT SAVE AREA)                
         ELSE                                                                   
           LHI  R0,WORKLEN         ALLOCATE EXIT WORK AREA                      
           GETMAIN R,LV=(0)                                                     
           LR   R7,R1              STORE A(SAVE AREA) IN EVENT BLOCK            
           ST   R7,EVUSER                                                       
         ENDIF                                                                  
*                                                                               
         USING WORKAREA,R7                                                      
*                                                                               
         ST    R7,8(RD)            LINK EXIT SAVEAREA TO SAVEAREA CHAIN         
         ST    RD,4(R7)                                                         
*                                                                               
         LR    RD,R7               SET NEW SAVEAREA AS CURRENT SAVEAREA         
*                                                                               
         L     RF,FLCCVT-PSA(,0)   A(CVT)                                       
         L     RF,CVTSMCA-CVT(,RF) SYSTEM MANAGEMENT CONTROL AREA: SMCA         
         LA    RE,LPARTAB          TABLE OF SFM SYSIDS                          
         USING LPARTABD,RE                                                      
         CLC   SMCASID-SMCABASE(,RF),LPARSMF FIND IT IN TABLE                   
         BE    GOTLPAR             GOT IT                                       
         LA    RE,LPARTABQ(,RE)                                                 
         CLI   0(RE),X'FF'         EOT?                                         
         BNE   *-18                                                             
*                                                                               
         MVI   WTOMSG,C' '         SMFID NOT FOUND IN TABLE !?!                 
         MVC   WTOMSG+1(L'WTOMSG-1),WTOMSG                                      
         MVC   WTOMSG(34),=C'PVRACF FATAL ERROR: UNKNOWN SMFID.'                
         MVC   WTOMSGL,=AL2(L'WTOMSG)                                           
         LA    R8,WTOMSGL          WTO MESSAGE LENGTH AND TEXT                  
         MVC   WWTO(@WTOL),#WTO    MOVE WTO MODEL                               
         SR    R0,R0               IBM SAYS CLEAR R0 BEFORE WTO                 
         WTO   TEXT=(R8),MF=(E,WWTO)                                            
         MVI   EVRESPON,ABORT      SET ABORT RESPONSE CODE                      
         B     DONE                RETURN TO PANVALET                           
*                                                                               
GOTLPAR  DS    0H                                                               
         MVC   LPAR,LPARSYS        SAVE LPAR NAME (E.G., 'SY1 ')                
         MVC   LPARTYPE,LPARTYP    'T' (TEST) OR 'P' (PRODUCTION)               
         DROP  RE                                                               
*                                                                               
*&&DEBUG                                                                        
         MVC   WTOMSGL,=AL2(L'WTOMSG)                                           
         MVI   WTOMSG,C' '                                                      
         MVC   WTOMSG+1(L'WTOMSG-1),WTOMSG                                      
         MVC   WTOMSG(21),=CL21'SPVRACF: EXIT INVOKED'                          
         LA    R8,WTOMSGL                                                       
         MVC   WWTO(@WTOL),#WTO    MOVE WTO MODEL                               
         SR    R0,R0               IBM SAYS CLEAR R0 BEFORE WTO                 
         WTO   TEXT=(R8),MF=(E,WWTO)                                            
*&&                                                                             
*                                                                               
         CLC   EVEVENT,=CL8'$MEM001'                                            
         BNE   *+8                                                              
         BAS   RE,MEMEVNT          PROCESS $MEM001 EVENT                        
*                                                                               
DONE     DS    0H                                                               
         LR    R7,RD               RESET CALLER'S SAVE AREA...                  
         L     RD,4(R7)            ...AS THE CURRENT SAVE AREA                  
*                                                                               
         XC    8(4,RD),8(RD)       REMOVE EXIT SAVE AREA...                     
         XC    4(4,R7),4(R7)       ...FROM THE SAVE AREA CHAIN                  
*                                                                               
         CLC   EVEVENT,=CL8'TERM'  PANVALET EXIT TERM EVENT?                    
         BNE   RETURN              NO                                           
*                                                                               
         LHI   R0,WORKLEN          FREE EXIT WORK AREA                          
         FREEMAIN R,LV=(0),A=(R7)                                               
         SR    R7,R7                                                            
         ST    R7,EVUSER                                                        
*                                                                               
RETURN   DS    0H                                                               
*                                                                               
* RETURN WITH RETURN CODE OF ZERO                                               
*                                                                               
         RETURN (14,12),T,RC=0                                                  
         EJECT                                                                  
*                                                                               
*        PROCESS $MEM001 EVENT. SEE THE SUPERVISOR SERVICES AND MACRO           
*        INSTRUCTIONS MANUAL FOR FOR EXPLANATIONS OF THE VARIOUS                
*        PARAMETERS BEING SET HERE. THE EXIT USES THE RACROUTE MACRO            
*        TO DETERMINE IF RACF IS ACTIVE, AND USES RACROUTE AGAIN TO             
*        DETERMINE IF ACCESS IS ALLOWED.                                        
*                                                                               
*        THE PANVALET $MEM001 EVENT OCCURS WHEN A PANVALET USER                 
*        ATTEMPTS TO ACCESS A MEMBER ON A PANVALET LIBRARY OR                   
*        PROTECTION FILE. SEE THE CA-PANVALET SYSTEM MANAGEMENT GUIDE           
*        FOR DETAILS.                                                           
*                                                                               
MEMEVNT  DS    0H                                                               
         ST    RE,SAVERE                                                        
         MVC   RACSD,RACS          FILL LIST FORM OF RACROUTE(1.9)              
         RACROUTE REQUEST=STAT,                                        +        
               WORKA=AREA1,                                            +        
               RELEASE=1.9,                                            +        
               MF=(E,RACSD)                                                     
         LTR   RF,RF               CHECK RETURN CODE                            
         BZ    GETFUNC             RACF IS ACTIVE                               
*                                                                               
         MVI   WTOMSG,C' '                                                      
         MVC   WTOMSG+1(L'WTOMSG-1),WTOMSG                                      
         MVC   WTOMSG(34),=C'PVRACF FATAL ERROR: RACF INACTIVE.'                
         B     FAIL                RACF NOT ACTIVE: WHY ?!?!?                   
*                                                                               
GETFUNC  DS    0H                                                               
         LA    R8,=CL8'FUNCTION'   GET PANVALET FUNCTION                        
         BAS   RE,GETDATA          SCAN PV DATA BLOCK                           
         MVC   FUNCTION,0(R9)      SAVE THE PANVALET FUNCTION                   
*                                                                               
         LA    R8,=CL8'PENDACT'                                                 
         BAS   RE,GETDATA          SCAN PV DATA BLOCK                           
*                                                                               
         IF  CLC,=CL8'WRITE',EQ,0(R9)                                           
           LHI  R3,WRITEREQ        SET WRITE FLAG                               
           MVC  ACTION,=CL6'UPDATE'                                             
         ELSE                                                                   
           LHI  R3,READREQ         SET READ FLAG                                
           MVC  ACTION,=CL6'READ'                                               
         ENDIF                                                                  
*                                                                               
         LA    R8,=CL8'DSN'        GET DATA SET NAME                            
         BAS   RE,GETDATA          SCAN PV DATA BLOCK                           
         MVC   RACFDSN(3),=C'PV.'  MOVE IN PSEUDO-PROFILE PREFIX                
         MVC   RACFDSN+3(44-3),0(R9)  MOVE IN PORTION OF DSN THAT FITS          
         MVC   LPARPROF,RACFDSN    SET BASIS FOR LPAR-SPECIFIC PROFILE          
*                                                                               
         LA    R8,=CL8'VOLUME'     GET VOLUME SERIAL                            
         BAS   RE,GETDATA          SCAN PV DATA BLOCK                           
         MVC   RACFVOL,0(R9)       STORE ADDRESS IN RACF PARM LIST              
*                                                                               
         LA    R8,=CL8'MBRNAME'    GET MEMBER NAME                              
         BAS   RE,GETDATA          SCAN PV DATA BLOCK                           
         BAS   RE,MOVEMEM          BUILD MEMBER NAME INTO DATA SET              
*                                                                               
         CLI   LPARTYPE,LPARPROD   IS THIS A PRODUCTION LPAR?                   
         BNE   CHKSCAN             NO: DON'T CHECK PRODUCTION PROFILES          
*                                                                               
* DENY ACCESS TO A TEST-LPAR-SPECIFIC VOLUME FROM A PRODUCTION LPAR.            
* (E.G., IN THE U.S., DON'T ALLOW ACCESS TO SYT VOLUMES FROM SY1.)              
* BY DOING THE HARD-CODED CHECK BELOW, WE OBVIATE THE NEED TO CREATE            
* DISCRETE LPAR-SPECIFIC RACF PSEUDO-PROFILES.                                  
*                                                                               
* NOTE: THIS CHECK ASSUMES THAT WE HAVE A VOLSER NAMING CONVENTION IN           
* PLACE, VIZ.: A CONSTANT 3-CHARACTER VOLSER PREFIX THAT UNIQUELY               
* IDENTIFIES A TEST-LPAR VOLSER.                                                
*                                                                               
*&&US*&& CLC   =C'SYT',RACFVOL     IN U.S., TEST PACKS START WITH "SYT"         
*&&UK*&& CLC   =C'LP2',RACFVOL     IN U.K., TEST PACKS START WITH "LP2"         
         BE    NOACCESS            ATTEMPT TO ACCESS TEST PACK IN PROD.         
*                                                                               
* SCAN THE TIOT FOR A DDNAME WHICH WE BELIEVE WILL BE ALLOCATED IF AND          
* ONLY IF WE ARE RUNNING UNDER ISPF.                                            
*                                                                               
* WE WANT TO KNOW IF WE ARE RUNNING UNDER ISPF SO THAT WE CAN *ALLOW*           
* READ ACCESS ON A PRODUCTION LPAR TO ALL PAN LIBRARIES, BUT ONLY FOR           
* ISPF ACTIONS SUCH AS BROWSE (=P.1). IN *BATCH*, WE *DENY* READ ACCESS         
* ON A PRODUCTION LPAR TO NON-PRODUCTION PAN LIBRARIES (EXCEPT FOR A            
* PANSCAN, WHICH IS DEEMED HARMLESS REGARDLESS OF THE LPAR).                    
*                                                                               
* (NOTE THAT DEIS TRIED DOING THE ISPF CHECK BY EXAMINING FIELD                 
*  'ENPRODOP', AS PER THE PANVALET DOCUMENTATION. UNFORTUNATELY, THIS           
*  METHOD WAS FOUND TO BE UNRELIABLE WHEN RUNNING UNDER JOHN                    
*  MCCONNELL'S CUSTOM MSL PANEL. THAT'S WHY WE SCAN THE TIOT INSTEAD.)          
*                                                                               
         CLC   =CL8'SCAN',FUNCTION IS THIS A ++SCAN COMMAND?                    
         BE    CHKSCAN             YES: LPAR-SPECIFIC AUTH NOT REQUIRED         
*                                                                               
         LA    RA,TIOT             GET A(TIOT)                                  
         EXTRACT (RA),'S',FIELDS=TIOT,MF=(E,LFXTRACT)                           
         L     RA,TIOT                                                          
         USING TIOT1,RA                                                         
         LA    RA,TIOENTRY         POINT TO DDNAME TABLE                        
         USING TIOENTRY,RA                                                      
CHKISPF  DS    0H                                                               
         CLI   TIOELNGH,0          EOT?                                         
         BE    CHKPSEUD            YES: WE AREN'T RUNNING UNDER ISPF            
         CLC   =CL8'ISPPROF',TIOEDDNM    LOOK FOR ISPF PROFILE ALLOC.           
         BE    CHKSCAN             FOUND IT: ALLOW ACCESS FROM ANY LPAR         
         LLC   R0,TIOELNGH         TRY NEXT TIOT TABLE ENTRY                    
         AR    RA,R0                                                            
         B     CHKISPF                                                          
         DROP  RA                                                               
*                                                                               
CHKPSEUD DS    0H                                                               
*                                                                               
* CONSTRUCT AN LPAR-SPECIFIC PSEUDO-PROFILE NAME. THE NAME WILL BE:             
*   'PV.[LIBRARY].[MEMBER].[LPAR]'                                              
*   E.G.: 'PV.PAN.DEIS.LIBRARY.SRCON00.SY1'                                     
*                                                                               
* NOTE THAT UNLIKE THE "ORIGINAL" PSEUDO-PROFILE NAMES WHICH WE'VE BEEN         
* USING FOREVER (AND CONTINUE TO USE), WE NEVER TRUNCATE ANY CHARACTERS         
* FROM THE PAN LIBRARY DSN IN THESE NEW LPAR-SPECIFIC PROFILE NAMES.            
* IF THE FULLY-CONSTRUCTED PROFILE NAME EXCEEDS 44 CHARACTERS, THEN WE          
* TRUNCATE THE *MEMBER* NAME INSTEAD. WE ALWAYS LEAVE ROOM FOR AT LEAST         
* FIVE CHARACTERS IN THE MEMBER NAME, AND FOUR CHARACTERS FOR THE               
* MAXIMUM POSSIBLE LPAR NAME. THEREFORE: THE LONGEST POSSIBLE PANVALET          
* LIBRARY NAME SUPPORTED BY THESE PROFILES IS 30 CHARACTERS.                    
*                                                                               
         LHI   R0,L'LPARPROF                                                    
         LA    R1,LPARPROF         BUILD LPAR-SPECIFIC PROFILE HERE             
         CLI   0(R1),C' '          FIND FIRST BLANK IN PV.DSN                   
         BE    *+12                GOT IT                                       
         LA    R1,1(,R1)                                                        
         BCT   R0,*-12                                                          
*                                                                               
         LA    RE,LPARPROF         POINT TO START OF DSN                        
         LR    R0,R1               POINT TO BLANK                               
         SR    R0,RE               GET LENGTH OF DSN WITH '.PV'                 
         CHI   R0,L'LPARPROF-(1+5+1+4)  + '.MEMBR' + '.LPAR'                    
         BNH   MOREPROF            LENGTH OKAY: KEEP BUILDING PROFILE           
*                                                                               
         MVI   WTOMSG,C' '                                                      
         MVC   WTOMSG+1(L'WTOMSG-1),WTOMSG                                      
         MVC   WTOMSG(51),=C'PVRACF FATAL ERROR: PANLIB DSN MAX LENGTH +        
               EXCEEDED.'                                                       
         B     FAIL                PANLIB L'DSN > 30 CHARACTERS !               
*                                                                               
MOREPROF DS    0H                                                               
*                                                                               
* GIVEN WHAT SPACE WE HAVE LEFT, FIGURE OUT HOW MANY CHARACTERS OF THE          
* MEMBER NAME WE CAN FIT, AND PUT THAT MUCH IN (UP TO EIGHT CHARACTERS          
* MAXIMUM). LEAVE ROOM FOR LPAR.                                                
*                                                                               
         MVI   0(R1),C'.'          DELIMIT THE MEMBER NAME QUALIFIER            
         LHI   RE,L'LPARPROF-(1+4+1) 44 MINUS LENGTH OF '.' + '.LPAR'           
         SR    RE,R0               RE = AVAILABLE SPACE FOR MEMBER NAME         
         CHI   RE,8                MAX L'MEMBERNAME WE CAN USE                  
         BL    *+8                 MUST WE TRUNCATE THE MEMBER NAME?            
         LHI   RE,8                NO                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),0(R9)       MOVE IN PART OF MBR. NAME THAT FITS          
         LA    R1,LPARPROF+L'LPARPROF                                           
         BCTR  R1,0                BACK UP TO LAST MEMBER NAME BYTE             
         CLI   0(R1),C' '                                                       
         BE    *-6                                                              
         MVI   1(R1),C'.'          DELIMIT THE LPAR QUALIFIER                   
         MVC   2(4,R1),LPAR        MOVE IN LPAR NAME (E.G., 'SY1 ')             
*                                                                               
*&&DEBUG                                                                        
         MVC   WTOMSGL,=AL2(L'WTOMSG)                                           
         MVC   WTOMSG,DEBUGMSG                                                  
         MVC   (DEBUGACT-DEBUGMSG)+WTOMSG,ACTION                                
         MVC   (DEBUGPRF-DEBUGMSG)+WTOMSG,LPARPROF                              
         LA    R8,WTOMSGL                                                       
         MVC   WWTO(@WTOL),#WTO    MOVE WTO MODEL                               
         SR    R0,R0               IBM SAYS CLEAR R0 BEFORE WTO                 
         WTO   TEXT=(R8),MF=(E,WWTO)                                            
*&&                                                                             
*                                                                               
* CHECK THE LPAR-SPECIFIC PSEUDO-PROFILE TO SEE IF ACCESS IS PERMITTED.         
*                                                                               
         LA    RA,LPARPROF         LPAR-SPECIFIC RESOURCE                       
         LA    R9,RACFVOL          POINT TO VOLUME                              
         MVC   RACRD,RACR          FILL LIST FORM OF RACROUTE(1.9)              
         RACROUTE REQUEST=AUTH,                                        +        
               WORKA=AREA1,                                            +        
               ENTITY=((RA)),      RA = A(PSEUDO-PROFILE NAME)         +        
               VOLSER=(R9),        REQUIRED PARAMETER                  +        
               ATTR=(R3),          READ REQUEST VS. WRITE REQUEST      +        
               RELEASE=1.9,                                            +        
               MSGSUPP=NO,         PRODUCE CONSOLE MESSAGES (DEFAULT)  +        
               MF=(E,RACRD)                                                     
         LTR   RF,RF               CHECK RETURN CODE                            
         BNZ   NOACCESS            ACCESS IS NOT ALLOWED                        
*                                                                               
CHKSCAN  DS    0H                                                               
*                                                                               
*        THE FORMAT OF THE RACF PARAMETER LIST IS AS FOLLOWS:                   
*                                                                               
*   ENTITY    DC  A           POINTER TO DATASET NAME                           
*   VOLSER    DC  A           POINTER TO VOLUME SERIAL                          
*   CLASS     DC  C'DATASET'  INDICATES DATASET ACCESS                          
*   ATTR      DC  XL1         X'02' - READ REQUEST                              
*                             X'04' - WRITE REQUEST                             
*                                                                               
         LA    RA,RACFDSN          POINT TO DSN                                 
         LA    R9,RACFVOL          POINT TO VOLUME                              
*                                                                               
*  IF THE EXIT IS INVOKED VIA A ++SCAN FUNCTION, WE INVOKE THE RACROUTE         
*  MACRO WITH THE LOG=NONE PARAMETER (THIS IS AN APF-AUTHORIZED CALL).          
*  THIS ALLOWS US TO CHECK READ AUTHORIZATION ON THE CURRENT MEMBER             
*  WITHOUT PRODUCING A RACF VIOLATION. THIS, IN TURN, ALLOWS US TO              
*  IMPLEMENT MEMBER-LEVEL RACF PROFILES WITH UACC=NONE ON A PRODUCTION          
*  PANVALET LIBRARY, WITHOUT CAUSING MEANINGLESS RACF VIOLATIONS                
*  RESULTING FROM INVOLUNTARY ATTEMPTS TO SCAN READ-PROTECTED MEMBERS.          
*  IF RACROUTE SAYS THAT READ ACCESS IS UNAUTHORIZED, THEN THIS EXIT            
*  PREVENTS THE MEMBER FROM BEING SCANNED.                                      
*                                                                               
         CLC   =CL8'SCAN',FUNCTION IS THIS A ++SCAN COMMAND?                    
         BNE   CONFAUTH            NO: PERFORM RACF AUTHORIZATION               
         CHI   R3,READREQ          IS READ PENDING?                             
         BE    CHK4IDF                                                          
*                                                                               
         MVI   WTOMSG,C' '                                                      
         MVC   WTOMSG+1(L'WTOMSG-1),WTOMSG                                      
         MVC   WTOMSG(38),=C'PVRACF FATAL ERROR: UNEXPECTED ACTION.'            
         B     FAIL                NO: ABORT (HOW COULD IT BE A WRITE?)         
*                                                                               
CHK4IDF  DS    0H                                                               
         CSVQUERY INEPNAME=ASMIDF,MF=(E,CSVQAREA)  ARE WE UNDER IDF?            
         LTR   RF,RF                                                            
         BZ    CONFAUTH            YES: SKIP APF-AUTHORIZED MACRO CALL          
*                                                                               
* THIS WILL PREVENT ++SCAN COMMANDS FROM CAUSING RACF VIOLATIONS, BUT           
* WE STILL NEED TO PREVENT UNAUTHORIZED USERS FROM READING ANY                  
* PANVALET MEMBER WITH A PROFILE OF UACC=NONE.                                  
*                                                                               
         MVC   RACRD,RACR          FILL LIST FORM OF RACROUTE(1.9)              
         RACROUTE REQUEST=AUTH,                                        +        
               WORKA=AREA1,                                            +        
               ENTITY=((RA)),      RA = A(PSEUDO-PROFILE NAME)         +        
               VOLSER=(R9),        REQUIRED PARAMETER                  +        
               ATTR=(R3),          READ REQUEST VS. WRITE REQUEST      +        
               RELEASE=1.9,                                            +        
               MF=(E,RACRD),                                           +        
               LOG=NONE            *** APF-AUTHORIZED PARAMETER ***             
         CHI   RF,4                IS USER ALLOWED TO READ THIS MEMBER?         
         BH    NOACCESS            NO: DON'T SCAN IT!                           
*                                                                               
CONFAUTH DS    0H                                                               
*                                                                               
*&&DEBUG                                                                        
         MVC   WTOMSGL,=AL2(L'WTOMSG)                                           
         MVC   WTOMSG,DEBUGMSG                                                  
         MVC   (DEBUGACT-DEBUGMSG)+WTOMSG,ACTION                                
         MVC   (DEBUGPRF-DEBUGMSG)+WTOMSG,RACFDSN                               
         LA    R8,WTOMSGL                                                       
         MVC   WWTO(@WTOL),#WTO    MOVE WTO MODEL                               
         SR    R0,R0               IBM SAYS CLEAR R0 BEFORE WTO                 
         WTO   TEXT=(R8),MF=(E,WWTO)                                            
*&&                                                                             
*                                                                               
         MVC   RACRD,RACR          FILL LIST FORM OF RACROUTE(1.9)              
         RACROUTE REQUEST=AUTH,                                        +        
               WORKA=AREA1,                                            +        
               ENTITY=((RA)),      RA = A(PSEUDO-PROFILE NAME)         +        
               VOLSER=(R9),        REQUIRED PARAMETER                  +        
               ATTR=(R3),          READ REQUEST VS. WRITE REQUEST      +        
               RELEASE=1.9,                                            +        
               MF=(E,RACRD)                                                     
         LTR   RF,RF               CHECK RETURN CODE                            
         BZ    MEMBEROK            ACCESS ALLOWED                               
*                                                                               
* RF = 4 IF THERE IS NO RACF PROFILE FOUND TO MATCH THE RESOURCE.               
* WE LET THESE PASS.                                                            
*                                                                               
         CHI   RF,4                RACF PROFILE FOUND?                          
         BE    MEMBEROK            NO: ALLOW ACCESS ANYWAY                      
*                                                                               
NOACCESS DS    0H                                                               
*                                                                               
* R5 MUST BE POINTING TO THE 'MBRNAME' DATA-ID NOW!                             
*                                                                               
         MVI   DLESTAT,INACTIVE    ACCESS TO THIS MEMBER IS DENIED              
*                                                                               
MEMBEROK DS    0H                                                               
         L     RE,SAVERE                                                        
         BR    RE                  RETURN TO CALLER                             
         EJECT                                                                  
*                                                                               
* MOVEMEM <==> FORMAT THE MEMBER NAME INTO THE DSN POINTED TO BY RACF           
*                                                                               
* INVOCATION SEQUENCE: R9 --> MEMBER NAME PASSED TO GEM EXIT                    
*                      BAS   RE,MOVEMEM                                         
*                      R9 --> DSN TO PASSED TO RACF                             
*                                                                               
* THE DSN TO BE PASSED TO RACF WILL HAVE A PV. APPENDED TO THE                  
* BEGINNING OF THE DSN PASSED TO GEM. I.E., IF GEM GETS PASSED                  
* 'PAN.DEIS.LIBRARY', THEN GEM WILL PASS 'PV.PAN.DEIS.LIBRARY' TO RACF          
* FOR VALIDATION. NOTE:                                                         
* THE DSN WILL ALSO HAVE A .MEMBERNAME APPENDED TO THE END. THIS IS             
* FOR THE OPTIONAL MEMBER LEVEL SECURITY.                                       
*                                                                               
* NOTE THAT IF THERE ISN'T ENOUGH ROOM FOR A TEN-CHARACTER MEMBER NAME,         
* THEN PART OF THE DSN WILL BE OVERWRITTEN WHEN THE RACF PROFILE NAME           
* IS CONSTRUCTED!                                                               
*                                                                               
MOVEMEM  DS    0H                                                               
         LHI   R0,L'RACFDSN                                                     
         LA    R1,RACFDSN                                                       
         CLI   0(R1),C' '          FIND FIRST BLANK IN PV.DSN                   
         BE    *+16                                                             
         LA    R1,1(,R1)                                                        
         BCT   R0,*-12                                                          
         B     MOVEOVER            NO BLANKS: CLOBBER PART OF DSN               
*                                                                               
         LA    RF,RACFDSN          POINT TO START OF DSN                        
         LR    R0,R1               POINT TO BLANK                               
         SR    R0,RF               GET LENGTH OF DSN + '.PV'                    
         CHI   R0,L'RACFDSN-(10+1) DO WE NEED SPACE FOR MEMBERNAME?             
         BNL   MOVEOVER            YES, CLOBBER PART OF DSN                     
         B     MOVEPER             NO, MOVE PERIOD + MEMBER NAME                
*                                                                               
MOVEOVER LA    R1,RACFDSN+(L'RACFDSN-(10+1)) CLOBBER LAST 11 DSN CHARS.         
MOVEPER  MVI   0(R1),C'.'          MOVE PERIOD TO DELIMIT QUALIFIER             
         MVC   1(10,R1),0(R9)      MOVE MEMBER NAME (LAST QUALIFIER)            
         BR    RE                  RETURN                                       
         EJECT                                                                  
*                                                                               
* GETDATA <==> SCAN PV DATA BLOCK FROM THE PANVALET DATA LIST BLOCK             
*                                                                               
* INVOCATION SEQUENCE: R8 --> DATA LIST IDENTIFIER (CL8)                        
*                      BAS   RE,GETDATA                                         
*                      R5 --> DATA LIST ENTRY                                   
*                      R9 --> DATA VALUE                                        
*                                                                               
* IF THE DATA ID IS NOT FOUND (INDICATING AN INTERNAL ERROR IN THE              
* EXIT), THIS SUBROUTINE WILL SET THE RESPONSE CODE AS "ABORT" AND              
* RETURN DIRECTLY TO THE MAINLINE ROUTINE.                                      
*                                                                               
GETDATA  DS    0H                                                               
         L     R0,DLCOUNT          A(FIRST ENTRY IN THE DATA LIST)              
         LA    R5,DLFIRST                                                       
*                                                                               
CHKNEXT  DS    0H                                                               
         LTR   R0,R0               LOOK FOR MATCH ON DATA ID                    
         BZ    CHKSRCH                                                          
         CLC   DLEID,0(R8)                                                      
         BE    CHKSRCH                                                          
*                                                                               
         LA    R5,DLENEXT          GET NEXT ENTRY IN THE DATA LIST              
         BCTR  R0,0                                                             
         B     CHKNEXT                                                          
*                                                                               
CHKSRCH  DS    0H                                                               
         LTR   R0,R0               DATA ID FOUND?                               
         BNZ   GETDATAX            YES: THAT'S GOOD                             
*                                                                               
         MVI   WTOMSG,C' '                                                      
         MVC   WTOMSG+1(L'WTOMSG-1),WTOMSG                                      
         MVC   WTOMSG(36),=C'PVRACF FATAL ERROR: UNKNOWN DATA ID.'              
         B     FAIL                INTERNAL ERROR!                              
*                                                                               
GETDATAX DS    0H                                                               
         L     R9,DLEITEM          R9= A(DATA ITEM VALUE)                       
         BR    RE                  RETURN TO CALLER                             
*                                                                               
FAIL     DS    0H                                                               
*                                                                               
* FATAL ERROR. PRODUCE A CONSOLE MESSAGE (PRESUMED TO BE PRESET IN              
* FIELD WTOMSG), AND TELL PANVALET TO ABORT PROCESSING.                         
*                                                                               
         MVC   WTOMSGL,=AL2(L'WTOMSG)                                           
         LA    R8,WTOMSGL          WTO MESSAGE LENGTH AND TEXT                  
         MVC   WWTO(@WTOL),#WTO    MOVE WTO MODEL                               
         SR    R0,R0               IBM SAYS CLEAR R0 BEFORE WTO                 
         WTO   TEXT=(R8),MF=(E,WWTO)                                            
*                                                                               
         MVI   EVRESPON,ABORT      SET ABORT RESPONSE CODE                      
         L     RE,SAVERE                                                        
         BR    RE                  RETURN TO PROGRAM MAINLINE                   
         EJECT                                                                  
RACS     RACROUTE REQUEST=STAT,RELEASE=1.9,MF=L                                 
RACSLEN  EQU   *-RACS                                                           
*                                                                               
RACR     RACROUTE REQUEST=AUTH,CLASS='DATASET',MF=L                             
RACRLEN  EQU   *-RACR                                                           
*                                                                               
EXTRAREA EXTRACT MF=L                                                           
EXTRACLQ EQU   *-EXTRAREA                                                       
*                                                                               
#WTO     WTO   TEXT=,MF=L          LIST FORM OF WTO                             
@WTOL    EQU   *-#WTO              LENGTH OF MODEL AREA                         
*                                                                               
DEBUGMSG DC    0CL78                                                            
         DC    C'SPVRACF: CALL RACROUTE TO '                                    
DEBUGACT DS    CL6                                                              
         DC    C': '                                                            
DEBUGPRF DS    CL44                                                             
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
ASMIDF   DC    CL8'ASMIDF'         DEBUGGER EXECUTABLE NAME                     
*                                                                               
LPARTAB  DS    0C                  SEE LPARTABD                                 
*&&US                                                                           
         DC    CL4'SYC',CL4'SY1',AL1(LPARPROD)                                  
         DC    CL4'SYA',CL4'SY7',AL1(LPARTEST)                                  
         DC    CL4'SYT',CL4'SYT',AL1(LPARTEST)                                  
*&&                                                                             
*&&UK                                                                           
         DC    CL4'SY1',CL4'SY1',AL1(LPARPROD)                                  
         DC    CL4'SY7',CL4'SY7',AL1(LPARTEST)                                  
         DC    CL4'SY2',CL4'SY2',AL1(LPARTEST)                                  
*&&                                                                             
         DC    X'FF'               EOT                                          
         EJECT                                                                  
WORKAREA DSECT                                                                  
SAVEAREA DS    18F                 OS SAVE AREA                                 
DUB      DS    D                                                                
TIOT     DS    D                                                                
SAVERE   DS    A                                                                
*                                                                               
WTOMSGL  DS    HL2                 FOR WTO                                      
WTOMSG   DS    CL(L'DEBUGMSG)                                                   
*                                                                               
RACFDSN  DS    CL44                                                             
LPARPROF DS    CL44                LPAR-SPECIFIC PSEUDO PROFILE                 
RACFVOL  DS    CL6                                                              
FUNCTION DS    CL8                 PANVALET FUNCTION                            
READREQ  EQU   X'02'               RACF CONSTANT: READ REQUEST                  
WRITEREQ EQU   X'04'               RACF CONSTANT: WRITE REQUEST                 
ACTION   DS    CL6                 'READ' OR 'UPDATE'                           
LPAR     DS    CL4                                                              
LPARTYPE DS    C                   TEST OR PRODUCTION                           
LPARTEST EQU   C'T'                                                             
LPARPROD EQU   C'P'                                                             
RACSD    DS    XL(RACSLEN)                                                      
RACRD    DS    XL(RACRLEN)                                                      
LFXTRACT DS    XL(EXTRACLQ)                                                     
AREA1    DS    XL512                                                            
*                                                                               
         CSVQUERY MF=(L,CSVQAREA),PLISTVER=MAX                                  
*                                                                               
WWTO     WTO   TEXT=,MF=L                                                       
*                                                                               
WORKLEN  EQU   *-WORKAREA                                                       
         SPACE 3                                                                
*                                                                               
*        LPAR TABLE                                                             
*                                                                               
LPARTABD DSECT                                                                  
LPARSMF  DS    CL4           SMF SYSTEM ID                                      
LPARSYS  DS    CL4           LPAR SYSTEM ID                                     
LPARTYP  DS    C             'T' (TEST) OR 'P' (PRODUCTION)                     
LPARTABQ EQU   *-LPARTABD                                                       
         SPACE 3                                                                
*                                                                               
*        EXIT PARAMETER LIST                                                    
*                                                                               
EXITPLST DSECT                                                                  
EXEVENT  DS    A             EVENT -- THE ADDRESS OF THE EVENT BLOCK            
EXENVIR  DS    A             ENVIRONMENT -- THE ADDRESS OF THE                  
*                            ENVIRONMENT BLOCK                                  
EXDATA   DS    A             DATA LIST -- THE ADDRESS OF THE DATA               
*                            LIST BLOCK (THE HIGH ORDER BIT IS SET              
*                            TO INDICATE THE END OF THE PARAMETER               
*                            ADDRESS LIST)                                      
         SPACE 3                                                                
*                                                                               
*        EVENT BLOCK                                                            
*                                                                               
EVENTBLK DSECT                                                                  
EVEVENT  DS    CL8           EVENT-ID -- THE NAME OF THE EVENT                  
*                            (E.G. INIT,$OPEN001,$MEM001,TERM ETC...)           
EVEXTYPE DS    CL8           EXIT TYPE -- ANY USER EXIT IDENTIFIER              
*                            (GIVEN ON THE EXIT TYPE= MACRO OR BLANKS)          
EVRESPON DS    CL1           RESPONSE CODE -- INITIALLY 'C', SET BY             
*                            USER EXIT TO:                                      
CONTINUE EQU   C'C'          'C' - CONTINUE NORMAL PROCESSING                   
ABORT    EQU   C'A'          'A' - ABORT PANVALET PROCESSING                    
TERMINAT EQU   C'T'          'T' - TERMINATE THIS EXIT                          
*                                  (PANVALET WILL DISCONNECT THE EXIT           
*                                  FROM ALL THE EVENTS FOR THE                  
*                                  DURATION OF THE RUNNING TASK ONLY            
*                                  AND PROCEEDS AS IF 'C' HAD BEEN              
*                                  SPECIFIED.  THIS INCLUDES THE TERM           
*                                  EVENT.  AN EXIT RESPONDING WITH A            
*                                  "T" SHOULD DO ALL CLEANUP REQUIRED           
*                                  BEFORE RETURNING.  OTHER EXIT(S),            
*                                  IF ANY, WILL CONTINUE TO BE CALLED           
*                                  BY PANVALET.)                                
VERIFY   EQU   C'V'          'V' - ACTIVATE VERIFICATION                        
*                                  (VALID ONLY FROM THE INIT EVENT;             
*                                  SEE THE INIT EVENT FOR DETAILS)              
         DS    CL3           RESERVED                                           
EVUSER   DS    A             USER FULLWORD -- INITIALLY ZEROES; TO BE           
*                            USED BY USER AS DESIRED (E.G. ADDRESS OF           
*                            A WORK AREA ETC...); THIS FIELD IS UNIQUE          
*                            TO EACH EXIT DEFINED BY A EXIT MACRO               
         SPACE 3                                                                
*                                                                               
*        ENVIRONMENT BLOCK                                                      
*                                                                               
ENVIRBLK DSECT                                                                  
ENPRODID DS    CL10          PRODUCT NAME -- VALUE IS ALWAYS "PANVALET"         
ENPRODOP DS    CL10          PRODUCT OPTION -- (E.G. TSO, ISPF)                 
ENPRODVR DS    CL4           PRODUCT VERSION -- THE RELEASE OF THE              
*                            PRODUCT (I.E. PANVALET 11.0 = "1100")              
ENOPSYS  DS    CL10          OPERATING SYSTEM -- THE OPERATING SYSTEM           
*                            UNDER WHICH THE PRODUCT IS RUNNING                 
*                            ("OS/MVS" OR "OS/VS1" OR "OS/MVS/XA")              
ENDCMON  DS    CL10          DC MONITOR                                         
ENDBMGR  DS    CL10          DB MANAGER                                         
         DS    XL2           RESERVED                                           
ENDCPARM DS    A             ADDRESS OF CICS ENVIRONMENT BLOCK OR 0             
ENDBPARM DS    A             ADDRESS OF DB MANAGER CONTROL BLOCK OR 0           
         SPACE 3                                                                
*                                                                               
*        DATA LIST BLOCK                                                        
*                                                                               
DATALIST DSECT                                                                  
DLCOUNT  DS    F             DATA LIST COUNT -- THE NUMBER OF DATA              
*                            LIST ENTRIES (0 TO N) FOLLOWING:                   
DLFIRST  DS    0X            FIRST DATA LIST ENTRY                              
*                                                                               
*        DATA LIST ENTRY                                                        
*                                                                               
DLENTRY  DSECT                                                                  
DLEID    DS    CL8           DATA ID -- THE NAME OF THE DATA ITEM               
*                            (E.G. ELEMFILE, JOBNAME ETC...)                    
DLESTAT  DS    CL1           DATA ITEM STATUS -- VALUES ARE:                    
ACTIVE   EQU   C'A'          'A' - ACTIVE                                       
INACTIVE EQU   C'I'          'I' - INACTIVE                                     
         DS    CL3           RESERVED                                           
DLEITEM  DS    A             DATA ITEM ADDRESS -- THE ADDRESS OF THE            
*                            ITEM                                               
DLENEXT  DS    0X            BEGINNING OF NEXT ENTRY                            
         SPACE 3                                                                
* CVT                                                                           
* IEESMCA                                                                       
* IEFTIOT1                                                                      
* ICHSAFP                                                                       
         PRINT OFF                                                              
         IHAPSA                                                                 
         EJECT                                                                  
         CVT   DSECT=YES                                                        
         EJECT                                                                  
         IEESMCA                                                                
         EJECT                                                                  
         DSECT                                                                  
         IEFTIOT1                                                               
         EJECT                                                                  
         ICHSAFP                                                                
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPVRACF   02/22/18'                                      
         END                                                                    
