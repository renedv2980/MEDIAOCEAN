         TITLE 'CONCRETE - OPERATIONS AND TECHNICAL GUIDE'                      
CONCRETE - CONTROL CARDS                                                        
*-----------------------                                                        
         INDEX 'CONCRETE - CONTROL CARDS'                                       
         SPACE 2                                                                
MODE=COPY           INDICATES THAT DATA IS TO BE COPIED FROM ONE STORAGE MEDIUM 
                    TO ANOTHER. BOTH INPUT AND OUTPUT CONTROL CARDS MUST BE     
                    SPECIFIED WITH THIS OPTION.                                 
         SPACE 1                                                                
MODE=REPORT         INDICATES THAT DATA IS TO BE READ FROM A STORAGE DEVICE.    
                    THIS OPTION IS USEFULL FOR RUNNING REPORT-TYPE OPERATIONS   
                    (I.E. TESTING EXTERNALS, DITTOING RECORDS ETC.). AN INPUT   
                    CONTROL CARD MUST BE SPECIFIED WITH THIS OPTION.            
         SPACE 1                                                                
INPUT=DISK          SPECIFIES THE INPUT STORAGE MEDIUM AS DISK. INPUT=DISK IS   
                    INVALID IF OUTPUT=DISK.                                     
         SPACE 1                                                                
INPUT=TAPE(,NNN)    SPECIFIES THE INPUT STORAGE MEDIUM AS TAPE AND OPTIONALLY   
                    DEFINES THE LOGICAL SYSNO OF THE DEVICE WHERE NNN IS A      
                    NUMBER BETWEEN 001 AND 255. IF NNN IS NOT SPECIFIED A DEF-  
                    AULT OF 010 IS ASSUMED. (NNN IS PATCHED INTO DTF+7).        
         SPACE 1                                                                
GENUP=YES           THIS OPTION WILL INSPECT THE FIRST RECORD READ FROM THE     
                    INPUT FILE. IF THIS RECORD IS A HEADER RECORD (X'00....01') 
                    THE FILE CONTROL ELEMENT WILL BE UPDATED WITH TODAY'S DATE  
                    AND THE GENERATION NUMBER WILL BE INCREMENTED BY 1. IF NOT  
                    AN ERROR MESSAGE AND DUMP WILL RESULT.                      
         SPACE 1                                                                
OVERRIDE=YES        WHEN COPYING A FILE FROM TAPE TO DISK, THE HEADER RECORD OF 
                    THE DISK FILE IS READ. IF THE GENERATION NUMBER OF THE INPUT
                    FILE IS NOT GREATER THAN THAT OF THE OUTPUT FILE AN ERROR   
                    MESSAGE WILL BE ISSUED AND THE RUN WILL CANCEL. THIS OPTION 
                    INHIBITS THE ABOVE CHECKING FEATURE.                        
         SPACE 1                                                                
LOAD='PHASE'        WHEN THIS CONTROL CARD IS ENCOUNTERED, THE CORE-IMAGE-      
                    LIBRARY PROGRAM (SPECIFIED BY PHASE) WILL BE LOADED IN AT   
                    THE END OF THE PARTITION IN WHICH CONCRETE IS RUNNING.      
                    CONTROL WILL BE PASSED TO THIS PHASE AFTER EACH RECORD THAT 
                    IS READ FROM THE INPUT DEVICE.                              
         SPACE 1                                                                
FILE='FINAME'       THIS WILL PATCH THE CONTROL FILE DTF (DTF+22) WITH THE NAME 
                    SPECIFIED BY FINAME. THIS ENABLES LOADING AND TESTING OF    
                    DUMMY FILES.                                                
         SPACE 1                                                                
RECCHECK=YES        WILL CHECK THE PHYSICAL CONSISTANCY OF ALL RECORDS IN THE   
                    FOLLOWING WAYS:-                                            
         SPACE 1                                                                
                    A) CHECK THAT THE RECORD IS NOT LONGER THAN 1000 BYTES AND  
                       NOT SHORTER THAN 49 BYTES LONG.                          
         SPACE 1                                                                
                    B) CHECKS THAT THERE ARE NO ELEMENTS OF ZERO LENGTH.        
         SPACE 1                                                                
                    C) CHECKS THAT THERE ARE NO LINKAGE ERRORS BETWEEN ELEMENTS.
                       (I.E. AN ELEMENT LINKS OUTSIDE THE SCOPE OF THE RECORD). 
         SPACE 1                                                                
                    IF ANY OF THE ABOVE CONDITIONS ARE TRUE, THE RECORD IS      
                    PRINTED OUT IN DITTO FORMAT AND THE NEXT RECORD IS READ FROM
                    THE INPUT DEVICE, THEREBY DELETING THE BAD RECORD.          
         SPACE 1                                                                
DITTO=YES           WILL PRODUCE A DISPLAY (IN DITTO FORMAT) OF ALL RECORDS READ
                    FROM THE INPUT DEVICE, PRECEEDED BY IT'S NAME AND LOGICAL   
                    NUMBER. THIS OPTION MAY BE USED WHEN REPORT=YES, HOWEVER    
                    OUTPUT WOULD BE MIXED. FOR SELECTIVE DISPLAYING USE START   
                    AND END CONTROL CARDS.                                      
         SPACE 1                                                                
DIRECTORY=YES       WILL PRODUCE A REPORT SHOWING THE KEYS OF ALL ACTIVE REC-   
                    ORDS ON THE CONTROL FILE.                                   
         SPACE 1                                                                
LIBREORG=YES        WILL REORGANISE/DELETE BOOK RECORDS (JCL/LIBRARY ETC.). THIS
                    OPTION IS ONLY VALID IF INPUT=DISK.                         
         SPACE 1                                                                
LIBREORG=PRINT      AS ABOVE BUT WILL ALSO PRINT ALL ACTIVE BOOKS.              
         SPACE 1                                                                
COUNT=YES           WILL PRODUCE A REPORT CATEGORIZING RECORDS BY STATUS & TYPE.
         SPACE 1                                                                
DELETE=YES          WILL PERFORM THE FOLLOWING FILE TIDY-UP FUNCTIONS:-         
         SPACE 1                                                                
                    A) DELETE AND PRINT UNKNOWN RECORDS. (IE THOSE WHICH ARE NOT
                       RECOGNIZED BY THE PROGRAM).                              
         SPACE 1                                                                
                    B) DELETE ALL RECORDS (EXCEPT BOOKS) WHICH HAVE THE X'80'   
                       BIT ON IN THE RECORD STATUS BYTE. (IE LOGICALLY DELETED).
         SPACE 1                                                                
                    C) DELETE EXPIRED TEMPORARY PROFILE ELEMENTS FROM REPORT    
                       PROFILE RECORDS.                                         
         SPACE 1                                                                
                    D) DELETE REPORT PROFILE RECORDS WHICH HAVE NO DATA.        
         SPACE 1                                                                
START=KEYEXP        WHEN INPUT=DISK, WILL START READING THE INPUT FILE FROM THE 
                    KEY SPECIFIED BY KEYEXP.(THIS DOES NOT HAVE TO BE A VALID   
                    CONTROL FILE RECORD). KEYEXP IS IN DECODE FORMAT (SEE       
                    DDMANUAL 'DECODE') AND IS THE SAME KEY DECODER USED IN PFM. 
                    IF START=KEYEXP IS NOT SPECIFIED A LOW-KEY VALUE OF X'00.01'
                    IS ASSUMED.                                                 
         SPACE 1                                                                
END=KEYEXP          AS ABOVE SPECIFYING AN END KEY. WHEN A RECORD IS READ FROM  
                    THE INPUT DEVICE WHOSE KEY IS GREATER THAN THAT OF THE END  
                    KEY, A BRANCH IS MADE TO THE END-OF-FILE ROUTINES. IF END=  
                    KEYEXP IS NOT SPECIFIED A HIGH-KEY VALUE OF X'FF.FF' IS     
                    ASSUMED.                                                    
         SPACE 1                                                                
OUTPUT=DISK         WHEN MODE=COPY INDICATES THAT A FILE IS TO BE LOADED TO     
                    DISK. THE GENERATION NUMBER OF THE INPUT FILE MUST BE       
                    GREATER THAN THAT OF THE OUTPUT FILE (SEE OVERRIDE=YES).    
                    BOTH HEADER AND TRAILER RECORDS MUST BE PRESENT ON THE INPUT
                    FILE. INPUT RECORDS ARE SEQUENCE-CHECKED BEFORE WRITING.    
                    OUTPUT=DISK IS INVALID IF INPUT=DISK.                       
         SPACE 1                                                                
OUTPUT=TAPE(,NNN)   SPECIFIES THE OUTPUT STORAGE MEDIUM AS TAPE AND OPTIONALLY  
                    DEFINES THE LOGICAL SYSNO OF THE DEVICE WHERE NNN IS A      
                    NUMBER BETWEEN 001 AND 255. IF NNN IS NOT SPECIFIED A DEF-  
                    AULT OF 011 IS ASSUMED. (NNN IS PATCHED INTO DTF+7). NO     
                    SEQUENCE CHECKING IS PERFORMED, AND HEADER/TRAILER RECORDS  
                    DO NOT HAVE TO BE PRESENT.                                  
         SPACE 1                                                                
         EJECT                                                                  
CONCRETE - PROCESSING SEQUENCE                                                  
*-----------------------------                                                  
         INDEX 'CONCRETE - PROCESSING SEQUENCE'                                 
         SPACE 2                                                                
    CONCRETE IS SPLIT INTO FIVE SEPARATE GROUPS OF FUNCTIONS. THESE ARE AS FOLL-
OWS:-                                                                           
         SPACE 1                                                                
INITIALIZATION - VALIDATE CONTROL CARDS AND BUILD HOOK LISTS.                   
FIRST TIME     - OPENING FILES, CLEARING ACCUMS ETC.                            
PROCESS        - FOR EVERY RECORD READ.                                         
LAST TIME      - CLOSING FILES. PRINTING ACCUMS ETC.                            
GENERAL        - GENERAL ROUTINES.                                              
         SPACE 1                                                                
    IN THE FOLLOWING LISTS AN 'O' BESIDE A SEQUENCE NUMBER INDICATES A BRANCH-  
AND-LINK TO A SUB-ROUTINE THAT IS GENERATED AS THE RESULT OF A CONTROL CARD. AN 
'A' BESIDE A SEQUENCE NUMBER INDICATES A SUB-ROUTINE WHICH IS ALWAYS EXECUTED.  
         SPACE 2                                                                
INITIALIZATION ROUTINES                                                         
*----------------------                                                         
         SPACE 1                                                                
1A  READ CONTROL CARDS AND VALIDATE. SET HOOK SWITCHES.                         
         SPACE 1                                                                
2A  SEE IF CONTROL OPTIONS ARE COMPATIBLE. CANCEL IF NOT.                       
         SPACE 1                                                                
3A  BUILD CODE HOOK LIST FROM CONTROL CARD OPTIONS (HOOK SWITCHES).             
         SPACE 1                                                                
4A  SET STXIT PC ROUTINE.                                                       
         SPACE 1                                                                
5A  GO TO FIRST TIME ROUTINES.                                                  
         SPACE 2                                                                
FIRST TIME ROUTINES                                                             
*------------------                                                             
         SPACE 1                                                                
1A  OPEN INPUT FILE AND READ FIRST RECORD. (TINFRST/DINFRST)                    
         SPACE 1                                                                
2A  EXTRACT RECORD TYPE. (RECTYPE)                                              
         SPACE 1                                                                
3A  CHECK HEADER RECORD AND UPDATE GENERATION RECORD IF REQUIRED. (GENCHCK)     
         SPACE 1                                                                
4O  GO TO DIRECTORY LIST FIRST TIME ROUTINE. (DIRFRST)                          
         SPACE 1                                                                
5O  GO TO LIBRARY REORGANIZATION FIRST TIME ROUTINE. (LIBFRST)                  
         SPACE 1                                                                
6O  GO TO DELETE FIRST TIME ROUTINE. (DELFRST)                                  
         SPACE 1                                                                
7O  GO TO DITTO FIRST TIME ROUTINE. (DITFRST)                                   
         SPACE 1                                                                
8O  OPEN OUTPUT FILE AND IF DISK CHECK GENERATION NUMBERS. (DOUFRST/TOUFRST)    
         SPACE 1                                                                
9A  GO TO PROCESSING ROUTINES OMITTING READ. (PROCESS+4)                        
         SPACE 1                                                                
         SPACE 2                                                                
PROCESSING ROUTINES                                                             
*------------------                                                             
         SPACE 1                                                                
1A  READ A RECORD FROM INPUT FILE. (DINPROC/TINPROC)                            
         SPACE 1                                                                
2A  EXTRACT RECORD TYPE. (RECTYPE)                                              
         SPACE 1                                                                
3O  SEE IF RECORD IS PHYSICALLY CONSISTENT. (RECPROC)                           
         SPACE 1                                                                
4O  GO TO DIRECTORY LIST PROCESSING ROUTINE. (DIRPROC)                          
         SPACE 1                                                                
5O  GO TO LIBRARY REORGANIZATION PROCESSING ROUTINE. (LIBPROC)                  
         SPACE 1                                                                
6O  GO TO DELETE ROUTINE. (DELPROC)                                             
         SPACE 1                                                                
7O  PASS RECORD TO EXTERAL PHASE.                                               
         SPACE 1                                                                
8O  GO TO LOGICAL COUNTING ROUTINES. (CNTRPOC)                                  
         SPACE 1                                                                
9O  GO TO DITTO ROUTINE. (DITPROC)                                              
         SPACE 1                                                                
10O WRITE RECORD (IF NOT DELETED) TO OUTPUT FILE (IF DISK CHECK SEQUENCE).      
    (DOUPROC/TOUPROC)                                                           
         SPACE 1                                                                
11A GO TO PROCESSING ROUTINES. (PROCESS)                                        
         SPACE 2                                                                
LAST TIME ROUTINES                                                              
*-----------------                                                              
         SPACE 1                                                                
1A  CLOSE INPUT FILE. SEE IF EOV OR EOF. IF EOV RE-OPEN FILE AND GO TO PROCESS- 
    ING ROUTINES. (TINLAST)                                                     
2O  GO TO EXTERNAL PHASE WITH OVSWITCH=X'FF' FOR LAST TIME ACTIONS.             
         SPACE 1                                                                
3O  GO TO LOGICAL COUNT LAST TIME ROUTINE AND PRINT REPORT. (CNTLAST)           
         SPACE 1                                                                
4O  CLOSE OUTPUT FILE (IF TAPE). CHECK TRAILER RECORD LOADED AND PRINT SIZE     
    INFO (IF DISK). (TOULAST/DOULAST)                                           
         SPACE 1                                                                
5A  PRINT PHYSICAL RECORD TOTALS. ISSUE MESSAGE IF BAD RECORDS FOUND. (RECTOTS) 
         SPACE 1                                                                
6A  END-OF-JOB.                                                                 
         EJECT                                                                  
CONCRETE - JCL AND I/O REQUIREMENTS                                             
*----------------------------------                                             
         INDEX 'CONCRETE - JCL AND I/O REQUIREMENTS'                            
         SPACE 2                                                                
DTF NAME  DEVICE  USAGE                         JCL REQUIRED                    
*-------  & TYPE  -----                         ------------                    
         SPACE 2                                                                
TINT      TAPE    USED WHEN INPUT=TAPE(,NNN)    ASSGN CARD FOR SYSXXX WHERE     
          INPUT   CONTROL CARD IS SPECIFIED.    XXX=NNN ON CONTROL CARD. NNN    
                                                CAN BE OMITTED IF XXX=010.      
         SPACE 2                                                                
TOUT      TAPE    USED WHEN OUTPUT=TAPE(,NNN)   ASSGN CARD FOR SYSXXX WHERE     
          OUTPUT  CONTROL CARD IS SPECIFIED.    XXX=NNN ON CONTROL CARD. NNN    
                                                CAN BE OMITTED IF XXX=011.      
         SPACE 2                                                                
CONTROL   DISK    USED WHEN INPUT=DISK OR       DLBL AND EXTENT INFORMATION FOR 
   OR     VLIS    OUTPUT=DISK CONTROL CARDS     CONTROL OR 'FINAME' (SEE -      
'FINAME'          ARE SPECIFIED.                'CONCRETE - CONTROL CARDS' -    
                                                FILE='FINAME').                 
         SPACE 2                                                                
SORTWK1   DISK    USED WHEN RECONSTRUCTING      DLBL AND EXTENT INFORMATION FOR 
          SORT    THE CONTROL FILE.             SORTWK1 ON SYS001.              
         EJECT                                                                  
CONCRETE - SAMPLE JOB SET-UPS                                                   
*----------------------------                                                   
         INDEX 'CONCRETE - SAMPLE JOB SET-UPS'                                  
         SPACE 2                                                                
    TO DUMP THE CONTROL FILE TO TAPE, REORGANIZE BOOK RECORDS AND PRODUCE A     
LOGICAL COUNT REPORT.                                                           
         SPACE 1                                                                
 // JOB CTDUMP                                                                  
 // EXEC CONCRETE                                                               
 MODE=COPY                                                                      
 INPUT=DISK                                                                     
 OUTPUT=TAPE                                                                    
 LIBREORG=YES                                                                   
 COUNT=YES                                                                      
 /*                                                                             
 /&                                                                             
         SPACE 2                                                                
    TO LOAD THE CONTROL FILE, DROP DELETED RECORDS AND PRODUCE A DIRECTORY      
LIST.                                                                           
         SPACE 1                                                                
 // JOB CTLOAD                                                                  
 // EXEC CONCRETE                                                               
 MODE=COPY                                                                      
 INPUT=TAPE                                                                     
 OUTPUT=DISK                                                                    
 GENUP=YES                                                                      
 DELETE=YES                                                                     
 DIRECTORY=YES                                                                  
 /*                                                                             
 /&                                                                             
         SPACE 2                                                                
    TO RECONSTRUCT THE CONTROL FILE TO DISK.                                    
         SPACE 1                                                                
 // JOB CTRECON                                                                 
 // ASSGN SYS011,X'280'  INPUT RECOVERY TAPE                                    
 // TLBL RCVTAPE,'RECOVERY TAPE'                                                
 // DLBL SORTWK1,'SORT WORK',0,SD                                               
 // EXTENT SYS001,,,,12,1500                                                    
 // EXEC CONCRETE                                                               
 MODE=COPY                                                                      
 INPUT=TAPE              ON SYS010                                              
 OUTPUT=DISK                                                                    
 OVERRIDE=YES                                                                   
 LOAD=CONRECST           RECONSTRUCT PROGRAM                                    
 DELETE=YES              IF NORMALLY SPECIFIED ON LOAD                          
 GENUP=YES                                                                      
 COUNT=YES                                                                      
 /*                                                                             
 /&                                                                             
         EJECT                                                                  
    TO DITTO TERMINAL RECORDS TO THE PRINTER.                                   
         SPACE 1                                                                
 // JOB CTDITTO                                                                 
 // EXEC CONCRETE                                                               
 MODE=REPORT                                                                    
 INPUT=DISK                                                                     
 DITTO=YES                                                                      
 START=(T)                                                                      
 END=(T)FF                                                                      
 /*                                                                             
 /&                                                                             
         SPACE 2                                                                
    TO COPY THE CONTROL FILE FROM TAPE TO TAPE.                                 
         SPACE 1                                                                
 // JOB CTCOPY                                                                  
 // EXEC CONCRETE                                                               
 MODE=COPY                                                                      
 INPUT=TAPE                                                                     
 OUTPUT=TAPE                                                                    
 /*                                                                             
 /&                                                                             
         EJECT                                                                  
CONCRETE - EXTERNAL PHASES                                                      
*-------------------------                                                      
         INDEX 'CONCRETE - EXTERNAL PHASES'                                     
         SPACE 2                                                                
    CONCRETE EXTERNAL PHASES SHOULD BE WRITTEN AS SELF-RELOCATING PHASES. NO    
WORKING-STORAGE SHOULD BE REQUIRED OTHER THAN THAT SUPPLIED BY CONCRETE. ON     
ENTERING AN EXTERNAL THE FOLLOWING REGISTERS ARE SET:-                          
         SPACE 1                                                                
R1 AND R6      POINT TO CONCRETE GLOBAL WORKING-STROAGE. THIS IS COVERED BY THE 
               DSECT ACCWORKD, THE FIRST STATEMENT AFTER AN NMOD1 IN AN EXTERNAL
               SHOULD BE A USING STATEMENT TO COVER THIS DSECT.                 
         SPACE 1                                                                
R8             CONTAINS THE ADDRESS OF THE PRINTER CSECT. THIS IS COVERED BY THE
               DSECT DPRINT.                                                    
         SPACE 2                                                                
    THE FOLLOWING DATA-NAMES CONTAINED IN THE ACCWORKD DSECT WILL BE OF MOST USE
TO CONCRETE EXTERNAL PROGRAMS.                                                  
         SPACE 1                                                                
OVSWITCH-PROCESS/LAST TIME INDICATOR.                                           
         SPACE 1                                                                
         X'00'=PROCESS A RECORD.                                                
         X'FF'=LAST TIME.                                                       
         SPACE 1                                                                
         IF A FIRST TIME HOOK IS ALSO REQUIRED, THE EXTERNAL CAN TEST FOR X'00',
         AND IF TRUE PERFORM FIRST TIME LOGIC AND SET OVSWITCH TO X'01'.        
         SPACE 1                                                                
WRITE   -WRITE/DELETE INDICATOR.                                                
         SPACE 1                                                                
         X'00'=WRITE CURRENT RECORD.                                            
         X'FF'=DELETE CURRENT RECORD.                                           
         SPACE 1                                                                
         AS DELETED RECORDS ARE ALWAYS PASSED TO EXTERNALS, THIS SWITCH MAY     
         ALREADY BE SET TO X'FF' ON ENTRY TO AN EXTERNAL. IF DELETED RECORDS ARE
         NOT TO BE PROCESSED A COMPARE AND BRANCH-IF-EQUAL SHOULD BE MADE AT THE
         BEGINNING OF THE EXTERNAL.                                             
         SPACE 1                                                                
INPDEV  -INPUT DEVICE INDICATOR  - X'80'=DISK, X'40'=TAPE.                      
         SPACE 1                                                                
OUTDEV  -OUTPUT DEVICE INDICATOR - X'80'=DISK, X'40'=TAPE, X'00'=NO OUTPUT.     
         SPACE 1                                                                
PROCREC - SET BY RECTYPE ROUTINE TO ONE OF THE FOLLOWING VALUES:-               
         SPACE 1                                                                
         AL1(00) - UNKNOWN RECORD TYPE.                                         
         AL1(01) - HEADER RECORD.                                               
         AL1(02) - TRAILER RECORD.                                              
         AL1(04) - SPOT DEMO CONTROL RECORD.                                    
         AL1(05) - ERROR MESSAGE RECORD.                                        
         AL1(08) - ALPHA USER-ID RECORD.                                        
         AL1(09) - OUTPUT TYPE RECORD.                                          
         AL1(10) - REPORT PROFILE RECORD.                                       
         AL1(14) - ALPHA TERMINAL RECORD.                                       
         AL1(15) - NUMERIC TERMINAL RECORD.                                     
         AL1(16) - NUMERIC USER-ID RECORD.                                      
         AL1(17) - USER PROFILE RECORD.                                         
         AL1(21) - LIBRARY (BOOK) RECORD.                                       
         AL1(22) - JCL (BOOK) RECORD.                                           
         SPACE 1                                                                
WORKEND -SUPPLIES EXTERNAL PHASES WITH 3000 BYTES OF PERMANENT WORKING-STORAGE. 
         THIS AREA IS NOT USED OR REFERENCED BY CONCRETE.                       
         SPACE 1                                                                
AIOAREA -CONTAINS THE ADDRESS OF THE CURRENT RECORD THAT IS BEING PROCESSED.    
         THIS ADDRESS-TYPE POINTS TO THE LENGTH OF THE RECORD. THEREFORE THE    
         FOLLOWING CODE SHOULD BE USED:-                                        
         SPACE 1                                                                
         L     R2,AIOAREA                                                       
         LA    R2,4(R2)                                                         
         USING CTXKEY,R2                                                        
         EJECT                                                                  
CONCRETE - SAMPLE EXTERNAL PROGRAM                                              
*---------------------------------                                              
         INDEX 'CONCRETE - SAMPLE EXTERNAL PROGRAM'                             
         SPACE 2                                                                
    THE FOLLOWING SAMPLE PROGRAM WILL PRINT-OUT THE KEYS OF ALL RECORDS (EXCEPT 
DELETES) IN HEX.                                                                
         SPACE 2                                                                
*PHASE CONTEST,+0,NOAUTO                                                        
*        PROGRAM TO LIST CONTROL FILE KEYS IN HEX FORMAT                        
*                                                                               
CONTEST  CSECT                                                                  
         NMOD1 0,**CONTEST                                                      
         USING CONWORKD,R6         R6=A(GLOBAL W/S)                             
         USING DPRINT,R8           R8=A(PRINT CSECT)                            
         CLI   OVSWITCH,X'FF'      IGNORE LAST TIME HOOK                        
         BE    EXIT                                                             
         CLI   WRITE,X'FF'         IGNORE DELETED RECORDS                       
         BE    EXIT                                                             
*                                                                               
         L     R2,AIOAREA          R2=A(RECORD-4)                               
         GOTO1 VHEXOUT,DMCB,4(R2),P,42,=C'TOG'                                  
         GOTO1 VPRINTER                                                         
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
*        INCLUDE CTCONWORKD                                                     
*        INCLUDE DDDPRINT                                                       
         EJECT                                                                  
CONCRETE - WORKING-STORAGE DSECT                                                
*-------------------------------                                                
         INDEX 'CONCRETE - WORKING-STORAGE DSECT'                               
         SPACE 2                                                                
*              DSECT TO COVER CONCRETE GLOBAL WORKING STORAGE                   
*                                                                               
CONWORKD DSECT                                                                  
         DS    CL8                 V-TYPE ROUTINES FOR I/O                      
VIVDDS   DS    V                   *                                            
VIVLDDS  DS    V                   *                                            
VIVREAD  DS    V                   *                                            
VIVRDSEQ DS    V                   *                                            
VIVADD   DS    V                   *                                            
VIVWRITE DS    V                   *                                            
VIVERASE DS    V                   *                                            
VCARDS   DS    V                   V-TYPE GENERAL AIDS                          
VCHOPPER DS    V                   *                                            
VCPRINT  DS    V                   *                                            
VKEYOUT  DS    V                   *                                            
VDECODE  DS    V                   *                                            
VDIVIDER DS    V                   *                                            
VDUMPOUT DS    V                   *                                            
VHELLO   DS    V                   *                                            
VHEXOUT  DS    V                   *                                            
VLOGIO   DS    V                   *                                            
VLOZENGE DS    V                   *                                            
VPRINT   DS    V                   *                                            
VPRINTER DS    V                   *                                            
         DS    CL8                 ADDRESSES OF DTFS                            
ACTFILE  DS    A                   *                                            
ATINT    DS    A                   *                                            
ATOUT    DS    A                   *                                            
         DS    CL8                 ADDRESSES OF I/O AREAS                       
AIOAREA  DS    A                   *                                            
AIOAREA2 DS    A                   MAY BE USED BY EXTERNAL                      
         DS    CL8                 ADDRESSES OF ELEMENTS                        
ADESCEL  DS    A                   *                                            
         DS    CL8                 ADDRESSES OF INTERNAL ROUTINES               
AFRIST   DS    A                   A(FIRST TIME CODE)                           
APROCESS DS    A                   A(PROCESS CODE)                              
ALAST    DS    A                   A(LAST TIME CODE)                            
APHASE   DS    A                   A(EXTERNAL PHASE)                            
         DS    CL8                 GENERAL PARAMETER LIST                       
DMCB     DS    6F                  *                                            
         DS    CL8                 PARAMETER LIST FOR IVDDS                     
IOPARM1  DS    A                   *                                            
IOPARM2  DS    A                   *                                            
IOPARM3  DS    A                   *                                            
IOPARM4  DS    A                   *                                            
IOPARM5  DS    A                   *                                            
IOPARM6  DS    A                   *                                            
         DS    CL8                 PARAMETER LIST FOR IVLDDS                    
IOPARMA  DS    A                   *                                            
IOPARMB  DS    A                   *                                            
IOPARMC  DS    A                   *                                            
IOPARMD  DS    A                   *                                            
IOPARME  DS    A                   *                                            
IOPARMF  DS    A                   *                                            
         DS    CL8                 PHYSICAL FILE COUNTS                         
PHINCNT  DS    PL4                 INPUT                                        
PHDECNT  DS    PL4                 PURGED                                       
PHOUCNT  DS    PL4                 OUTPUT                                       
PHBDCNT  DS    PL4                 BAD RECORDS                                  
         DS    CL8                 GENERATION NUMBERS                           
GENNIN   DS    CL3                 INPUT                                        
GENOUT   DS    CL3                 OUTPUT                                       
         DS    CL4                 I/O DEVICES                                  
INPDEV   DS    C                   INPUT  DEVICE X'80'=DSK,X'40'=TPE            
OUTDEV   DS    C                   OUTPUT DEVICE X'80'=DSK,X'40'=TPE            
         DS    CL8                 KEYS                                         
KEYSTRT  DS    CL25                *                                            
KEYEND   DS    CL25                *                                            
         DS    CL8                 WORK AREAS (TEMPORARY)                       
CDAREA   DS    CL80                *                                            
MSGAREA  DS    CL40                *                                            
DUB      DS    D                   *                                            
DUB2     DS    D                   *                                            
FULL     DS    F                   *                                            
FULL2    DS    F                   *                                            
HALF     DS    H                   *                                            
HALF2    DS    H                   *                                            
BYTE     DS    C                   *                                            
BYTE2    DS    C                   *                                            
WORK     DS    CL20                *                                            
         DS    CL8                 SWITCHES                                     
OVSWITCH DS    C                   FOR EXTERNAL (00=FIRST/FF=LAST)              
WRITE    DS    C                   SET TO X'FF' TO DELETE CURRENT REC           
PROCREC  DS    C                   TYPE OF CURRENT RECORD (SEE BELOW)           
         DS    CL8                 EXTERNAL PERMENANT WORK AREA                 
WORKEND  DS    0C                  3000 BYTES IN LENGTH                         
         EJECT                                                                  
CONCRETE - RECORD TYPE EQUATES                                                  
*-----------------------------                                                  
         INDEX 'CONCRETE - RECORD TYPE EQUATES'                                 
         SPACE 2                                                                
*              RECORD TYPE EQUATES                                              
*                                                                               
UNKNOWN  EQU   0                   *                                            
HEADER   EQU   1                   *                                            
TRAILER  EQU   2                   *                                            
*                                  GENERAL                                      
SPOTDEMO EQU   4                   *                                            
ERROR    EQU   5                   *                                            
FILECON  EQU   6                   *                                            
REQGROUP EQU   7                   *                                            
USERID   EQU   8                   *                                            
OUTPUT   EQU   9                   *                                            
PROFILE  EQU   10                  *                                            
REQPROTO EQU   11                  *                                            
REQUEST  EQU   12                  *                                            
SCREEN   EQU   13                  *                                            
TERMINAL EQU   14                  *                                            
USERPROF EQU   17                  *                                            
*                                  PASSIVE POINTERS                             
TERMPASS EQU   15                  *                                            
IDPASS   EQU   16                  *                                            
*                                  BOOKS                                        
COMMENT  EQU   20                  *                                            
LIBRARY  EQU   21                  *                                            
JCL      EQU   22                  *                                            
         EJECT                                                                  
CONCRETE - PRINTER MESSAGES                                                     
*--------------------------                                                     
         INDEX 'CONCRETE - PRINTER MESSAGES'                                    
         SPACE 3                                                                
MESSAGE                                  MEANING                                
*------                                  -------                                
         SPACE 2                                                                
CANCELLED DUE TO OPERATOR OPTION         THE CONSOLE OPERATOR REPLIED 'CANCEL'  
                                         TO INVALID CONTROL CARD MESSAGE. (SEE  
                                         'CONCRETE - CONSOLE MESSAGES').        
         SPACE 2                                                                
MISSING OR INVALID INPUT PARAMETER       THE INPUT=DEV CONTROL CARD WAS EITHER  
                                         INVALID AND IGNORED BY THE CONSOLE     
                                         OPERATOR, OR WAS MISSING FROM THE INPUT
                                         STREAM.                                
         SPACE 2                                                                
MISSING OR INVALID OUTPUT PARAMETER      AS ABOVE FOR OUTPUT=DEV.               
         SPACE 2                                                                
OUTPUT SPECIFIED FOR MODE=REPORT         EITHER MODE CARD HAS BEEN INCORRECTLY  
                                         SPECIFIED, OR OUTPUT=DEV INCLUDED IN   
                                         ERROR.                                 
         SPACE 2                                                                
MISSING OR INVALID MODE PARAMETER        THE MODE=OPTION CARD WAS EITHER INVALID
                                         AND IGNORED BY THE CONSOLE OPERATOR, OR
                                         WAS MISSING FROM THE INPUT STREAM.     
         SPACE 2                                                                
INCOMPATIBLE INPUT/OUTPUT PARAMETER      BOTH INPUT AND OUTPUT DEVICES WERE     
                                         SPECIFIED AS DISK.                     
         SPACE 2                                                                
START=KEY ONLY VALID IF INPUT=DISK       SELF EXPLANATORY.                      
         SPACE 2                                                                
LOAD NOT AUTHORIZED (GENERATION)         AN ATTEMPT WAS MADE TO LOAD A TAPE FILE
                                         TO DISK WHOSE GENERATION NUMBER WAS NOT
                                         GREATER THAN THAT OF THE OUTPUT FILE.  
                                         (SEE 'CONCRETE - CONTROL CARDS' -      
                                          OVERRIDE=YES).                        
         EJECT                                                                  
CONCRETE - CONSOLE MESSAGES                                                     
*--------------------------                                                     
         INDEX 'CONCRETE - CONSOLE MESSAGES'                                    
         SPACE 3                                                                
MESSAGE                                  MEANING                                
*------                                  -------                                
         SPACE 2                                                                
XXXXXXXXXXXXXXX                          IS ISSUED WHEN AN UNKNOWN OR INVALID   
INVALID CONTROL CARD                     CONTROL CARD IS ENCOUNTERD IN THE INPUT
PLEASE CORRECT,IGNORE OR CANCEL          STREAM (WHERE XXX IS THE CARD IN ERROR)
                                         THE CONSOLE OPERATOR CAN THEN:-        
         SPACE 1                                                                
                                         1. RE-INPUT THE CORRECT CONTROL CARD   
                                            THROUGH THE CONSOLE                 
         SPACE 1                                                                
                                         2. TYPE 'IGNORE' TO BYPASS THE CONTROL 
                                            CARD.                               
         SPACE 1                                                                
                                         3. TYPE 'CANCEL' TO CANCEL THE RUN.    
         SPACE 2                                                                
EOV/EOF ENQUIRY?                         IS ISSUED TO THE CONSOLE PRINTER-KB    
                                         WHEN AN END-OF-VOLUME CONDITION OCCURS 
                                         ON A TAPE UNIT. TYPE ONE OF THE FOLL-  
                                         OWING:-                                
         SPACE 1                                                                
                                         1. 'EOV' - IF ANOTHER TAPE IS TO BE    
                                            INPUT.                              
         SPACE 1                                                                
                                         2  'EOF' - IF THE TAPE CURRENTLY ON THE
                                            TAPE DRIVE IS THE LAST INPUT VOLUME.
         SPACE 2                                                                
ANY MORE RECOVERY TAPES TO BE MERGED ?   IS ISSUED AT EOF ON A RECOVERY TAPE    
                                         WHEN RECONSTRUCTING THE FILE. TYPE ONE 
                                         OF THE FOLLOWING -                     
         SPACE 1                                                                
                                         1. 'YES' - IF ANOTHER TAPE IS TO BE    
                                            INPUT.                              
         SPACE 1                                                                
                                         2. 'NO'  - IF NO MORE TAPES ARE TO BE  
                                            INPUT.                              
         SPACE 2                                                                
NUMBER OF TRACKS AVAILABLE=NNNN          IS ISSUED AFTER A FILE HAS BEEN LOADED.
                                         WHERE NNNN IS THE NUMBER OF PRIME DATA 
                                         TRACKS AVAILABLE FOR USE.              
         EJECT                                                                  
NUMBER OF INDEX BYTES REQUIRED=NNNN      IS ISSUED AFTER A FILE HAS BEEN LOADED.
                                         WHERE NNNN IS THE NUMBER OF BYTES REQU-
                                         IRED TO HOLD THE CONTROL FILE MASTER   
                                         INDEX IN CORE.                         
         SPACE 2                                                                
**CONCRETE** GENERATION RECORD MISSING   IS ISSUED WHEN THE FILE HEADER RECORD  
                                         IS MISSING FROM A FILE. THE RECORD THAT
                                         WAS READ WILL BE DISPLAYED ON THE      
                                         PRINTER (UNLESS THIS MESSAGE IS FOLL-  
                                         OWED BY 'SEE I/O2 IN DUMP').           
         SPACE 2                                                                
**CONCRETE** SEE I/O2 IN DUMP            IS ISSUED AFTER THE ABOVE MESSAGE. THIS
                                         INDICATES THAT THE HEADER RECORD OF THE
                                         OUTPUT FILE (WHEN OUTPUT=DISK) WAS     
                                         MISSING OR A DISK ERROR OCCURED. I/O2  
                                         IN THE RESULTANT DUMP WILL SHOW THE    
                                         RECORD THAT WAS READ.                  
         SPACE 2                                                                
**CONCRETE** TRAILER RECORD MISSING      IS ISSUED IF A FILE TRAILER RECORD WAS 
                                         NOT FOUND (WHEN OUTPUT=DISK) ON THE    
                                         INPUT FILE. THE LAST RECORD READ FROM  
                                         THE INPUT FILE IS DISPLAYED ON THE     
                                         PRINTER.                               
         SPACE 2                                                                
**CONCRETE** CANCELLED DUE TO I/O ERROR  IS ISSUED AFTER AN **CONCRETE** ERROR  
                                         MESSAGE IF AN UNRECOVERABLE ERROR IS   
                                         DETECTED, OR IF AN UNRECOVERABLE DISK  
                                         ERROR IS POSTED (SEE IOPARMS IN DUMP). 
         SPACE 2                                                                
**CONCRETE** WARNING BAD RECORDS FOUND   IS ISSUED AT THE END OF THE RUN IF ANY 
                                         RECOVERABLE ERRORS WERE DETECTED. BAD  
                                         RECORDS ARE PRINTED IN DITTO FORMAT    
                                         WITH AN ERROR MESSAGE WHICH INDICATES  
                                         THE FAULT.                             
         SPACE 2                                                                
**CONCRETE** RECORD OUT OF SEQUENCE      IS ISSUED AT LOAD TIME WHEN ADDING A   
                                         RECORD WHOSE KEY IS NOT GREATER THAN   
                                         THAT OF THE PREVIOUS RECORD.           
         EJECT                                                                  
