*          DATA SET DDASMXIT   AT LEVEL 010 AS OF 02/23/21                      
*PROCESS USING(WARN(15))                                                        
*PHASE DDASMXTA                                                                 
*SETOPT  PARM(REUS=RENT)                                                        
FLOWASM  RSECT ,                                                                
FLOWASM  AMODE 31                                                               
FLOWASM  RMODE 31                                                               
         ACONTROL COMPAT(NOCASE)                                                
         TITLE 'High Level Assembler Source and Listing Exits'                  
         PRINT NOGEN                                                            
         REQUS                                                                  
         EJECT ,                                                                
                                                                                
*************************** NOTES FROM DEIS ***************************         
*                                                                     *         
*  This module is a HLASM exit. It is essentially Ed Jaffe's FLOWASM  *         
*  exit, as obtained here:                                            *         
*                                                                     *         
*     ftp://ftp.phoenixsoftware.com/pub/demo/flowasm.zip              *         
*                                                                     *         
*  DEIS started with Ed Jaffe's FLOWASM source, then added some       *         
*  additional functionality (see "non-transparent" notes below).      *         
*                                                                     *         
*  The linked load module resides in 'DDS.LOADLIB'. As a result, the  *         
*  assembler step in our procs must STEPLIB to that load library, or  *         
*  else the exit will not be found, and the assemblies will fail.     *         
*  The exit is invoked by adding this PARM to the assembler EXEC      *         
*  statement in the JCL:                                              *         
*                                                                     *         
*     'EXIT(INX(DDASMXT),PRX(DDASMXT))'                               *         
*                                                                     *         
*  This program is invoked as a SOURCE and LISTING exit. Although     *         
*  FLOWASM does support a LIBRARY exit also, we do not make use of    *         
*  that functionality at this time.                                   *         
*                                                                     *         
*  NOTE:                                                              *         
*  The listing exit makes certain assumptions about the format of     *         
*  the printed listing line. If the format of the listing changes in  *         
*  future versions of the assembler, then this program may require    *         
*  modification. The exit *is* clever enough to handle wide-format    *         
*  listings (i.e., 133 columns, instead of the default of 121).       *         
*                                                                     *         
*  =================================================================  *         
*                                                                     *         
*  NON-transparent modifications from Ed Jaffe's original version of  *         
*  FLOWASM are:                                                       *         
*                                                                     *         
*    1. Not all statements are eligible for automatic continuation.   *         
*       The only macros which will be automatically continued are     *         
*       those in the FLOWDDSM table. This is because much of our      *         
*       legacy code has extraneous commas which look to FLOWASM as    *         
*       if they are positional macro parameters, and would therefore  *         
*       cause an unwanted (and potentially dangerous) automatic       *         
*       continuation.                                                 *         
*    2. Statements in error are NOT ignored for the purpose of        *         
*       adding flowbars to the listing. This approach is necessary    *         
*       because we are suppressing the ASMA169I message via our       *         
*       assembly options, but (despite the suppression of the         *         
*       message) the affected statements are flagged in error anyway  *         
*       by the time this exit sees them. As a result, the original    *         
*       FLOWASM code prevented such statements from getting           *         
*       flowbars. There doesn't seem to be an easy way to know that   *         
*       a statement was flagged due to the ASMA169I warning (as       *         
*       opposed to some other error), so we simply decided to print   *         
*       flowbars on ALL flagged statements. This seems harmless       *         
*       enough.                                                       *         
*    3. As an aid to the developer, the listing exit modifies the     *         
*       printing of SPM-generated jump instructions. Viz.: we print   *         
*       the extended mnemonic, rather than "BRC" with the branch mask *         
*       (because no one can remember what the branch masks mean).     *         
*       E.g., if an SPM generates a "JC 8,x" instruction, then the    *         
*       listing exit changes this to "JE" or "JZ", depending upon the *         
*       previous instruction. This listing line modification only     *         
*       happens if PRINT GEN is active (obviously).                   *         
*    4. We trap some specific severity 4 assembler warnings (see      *         
*       table WARNTAB) and raise their severity from 4 to 8.          *         
*                                                                     *         
*  Transparent modifications to Ed Jaffe's FLOWASM code:              *         
*                                                                     *         
*    1. Our own REQUS macro is used, not IBM'S ASMDREG macro.         *         
*    2. Register equates R10-R15 are changed to RA-RF.                *         
*    3. Destructive moves after MVCL will cause an abend. (Note that  *         
*        unless there's a bug, this abend should never occur.)        *         
*    4. COMPAT(NOCASE) is added to allow lowercase symbols.           *         
*    5. This module is flagged as reentrant for the binder.           *         
*    6. AHI instructions were changed to our SHI macro where          *         
*       appropriate.                                                  *         
*    7. Conditional assembly code has been removed involving non-z/OS *         
*       operating systems.                                            *         
*    8. SPMs: the IF, DOEXIT, and WHEN macro calls now have their     *         
*       conditional expressions surrounded by parentheses. (For       *         
*       simple expressions, this is apparently not actually required, *         
*       but all of the calls now do conform to the syntax shown in    *         
*       the IBM documentation.)                                       *         
*                                                                     *         
***********************************************************************         
*                                                             *                 
*START OF SPECIFICATIONS***************************************                 
*                                                             *                 
* MODULE NAME = FLOWASM                                       *                 
*                                                             *                 
* DESCRIPTIVE NAME = High Level Assembler Reformatting Exits  *                 
*                                                             *                 
* FUNCTION =                                                  *                 
*                                                             *                 
*    The source and library exits relax assembler language    *                 
*    syntax rules to allow a more "free form" coding style.   *                 
*    This is intended to make assembler language easier to    *                 
*    code and use, especially when indenting structured       *                 
*    programming macros.                                      *                 
*                                                             *                 
*    The listing exit will add flow bars to the program       *                 
*    listing to enhance readability when using the structured *                 
*    programming macros provided with IBM's HLASM Toolkit.    *                 
*                                                             *                 
* AUTHOR =                                                    *                 
*                                                             *                 
*    Edward E. Jaffe                                          *                 
*    Phoenix Software International                           *                 
*    5200 W. Century Blvd., Suite 800                         *                 
*    Los Angeles, CA 90045 U.S.A.                             *                 
*                                                             *                 
* ATTRIBUTES =                                                *                 
*                                                             *                 
*    Reentrant, Amode(31), Rmode(Any)                         *                 
*    Must be in APF Library if Invoked by SMP/E               *                 
*                                                             *                 
* OPERATION = Please Read the Following                       *                 
*                                                             *                 
*    ======================================================== *                 
*                                                             *                 
*    Assembler Specification:                                 *                 
*                                                             *                 
*    EXIT(INX(FLOWASM),LBX(FLOWASM),PRX(FLOWASM))             *                 
*                                                             *                 
*    All three specifications are not required. You can use   *                 
*    any exit by itself. For example, you can use just the    *                 
*    listing exit (PRX) to print flow bars without using the  *                 
*    enhanced syntax.                                         *                 
*                                                             *                 
*    ======================================================== *                 
*                                                             *                 
*    Input Formats When Using Source/Library Exits:           *                 
*                                                             *                 
*    Only fixed length records with LRECL=80 are supported    *                 
*    for library input (SYSLIB). Source input may be any of   *                 
*    the following:                                           *                 
*                                                             *                 
*    1) Fixed records of any size. Sequence numbers are       *                 
*    allowed in columns LRECL-7 through LRECL. The            *                 
*    continuation column is LRECL-8.                          *                 
*                                                             *                 
*    2) Variable length records without sequence numbers.     *                 
*    Continuation is indicated with a trailing '+' character. *                 
*                                                             *                 
*    3) Variable length records with sequence numbers in      *                 
*    columns 1-8. Continuation is indicated with a trailing   *                 
*    '+' character.                                           *                 
*                                                             *                 
*    4) Fixed length records "pretending" to be variable      *                 
*    length records without sequence numbers. This format is  *                 
*    useful when, for example, your variable length file is   *                 
*    input to the assembler in-stream via JCL. Trailing       *                 
*    blanks are ignored and processing is as for variable     *                 
*    length records without sequence numbers. To distinguish  *                 
*    this format from true fixed length records, include the  *                 
*    following as the first statement of the source module:   *                 
*                                                             *                 
*    *FLOWOPT RECFM=V                                         *                 
*                                                             *                 
*    5) Fixed length records "pretending" to be variable      *                 
*    length records with sequence numbers in columns 1-8.     *                 
*    This format is useful when, for example, your variable   *                 
*    length file is input to the assembler in-stream via JCL. *                 
*    Trailing blanks are ignored and processing is as for     *                 
*    variable length records with sequence numbers. To        *                 
*    distinguish this format from true fixed length records,  *                 
*    include the following as the first statement of the      *                 
*    source module:                                           *                 
*                                                             *                 
*    *FLOWOPT RECFM=V                                         *                 
*                                                             *                 
*    ======================================================== *                 
*                                                             *                 
*    Comment Block Identification:                            *                 
*                                                             *                 
*    Normally, the assembler requires comment blocks be       *                 
*    marked with an asterisk in column 1. With this exit,     *                 
*    comment blocks may begin anywhere on the line. They may  *                 
*    be denoted by an asterisk or by a /* sequence. The exit  *                 
*    recognizes these comments and supplies the asterisk in   *                 
*    column 1.                                                *                 
*                                                             *                 
*    ======================================================== *                 
*                                                             *                 
*    Blank Elimination / Wrapping of Long Lines:              *                 
*                                                             *                 
*    If a statement is too long to fit in a standard 80-byte  *                 
*    assembler source line, the statement is reformatted as   *                 
*    follows:                                                 *                 
*    1) All extraneous blanks between the operation code and  *                 
*       operand are eliminated.                               *                 
*    2) If the statement is still too long, extraneous blanks *                 
*       between the operand and commentary are removed.       *                 
*    3) If the statement is still too long, extraneous blanks *                 
*       in front of the operation code are removed.           *                 
*    4) If the statement is still too long:                   *                 
*       a) If the operand fits on the line, the commentary is *                 
*          truncated.                                         *                 
*       b) If the operand is too long to fit, it is wrapped   *                 
*          and continued in column 16 of the next line along  *                 
*          with any commentary.                               *                 
*                                                             *                 
*    ======================================================== *                 
*                                                             *                 
*    Automatic Continuation:                                  *                 
*                                                             *                 
*    Good coding standards usually expect macro operands to   *                 
*    be spread out, with one operand and associated           *                 
*    commentary per source line. The exit detects such        *                 
*    sequences and automatically supplies the '-'             *                 
*    continuation character. The '-' is chosen to distinguish *                 
*    it from '+' characters coded by the programmer.          *                 
*                                                             *                 
*    When continuation lines are coded, there is no need to   *                 
*    continue in column 16. The exit knows when it has        *                 
*    supplied continuation for the previous source line. The  *                 
*    continuation line is reformatted so that the operand     *                 
*    begins in column 16. (This may be either a left or right *                 
*    shift of the operand.) If the commentary must be moved   *                 
*    from its current position, either because it conflicts   *                 
*    with the new operand position or because the statement   *                 
*    is too long, it is moved immediately after the operand.  *                 
*    If the statement is still too long, wrapping and/or      *                 
*    comment truncation occurs as described above under       *                 
*    "Blank Elimination / Wrapping of Long Lines".            *                 
*                                                             *                 
*    ======================================================== *                 
*                                                             *                 
*    Manual Continuation:                                     *                 
*                                                             *                 
*    The blank elimination / wrapping and automatic continua- *                 
*    tion features are disabled for any line that is manually *                 
*    continued and for all subsequent continuation lines for  *                 
*    the same statement. Automatic continuation and manual    *                 
*    continuation may be mixed within a continued statement,  *                 
*    but once manual continuation begins, no more automatic   *                 
*    continuation is allowed.                                 *                 
*                                                             *                 
*    ======================================================== *                 
*                                                             *                 
*    Example Program:                                         *                 
*                                                             *                 
*    Freed from the confines of traditional assembler syntax, *                 
*    structured programming macros provide a considerably     *                 
*    more powerful and easy to use language. For example:     *                 
*                                                             *                 
*  ********************************************************   *                 
*  * Get Dynamic Working Storage                          *   *                 
*  ********************************************************   *                 
*    ICM R3,B'1111',DwsAddr         Get storage address       *                 
*    IF Z                           If not yet acquired       *                 
*      STORAGE OBTAIN,                Storage obtain          *                 
*              SP=1,                  - Subpool 1             *                 
*              LENGTH=DwsSize         - Length of request     *                 
*      ST    R1,DwsAddr               Save storage address    *                 
*      LR    R3,R1                    (same)                  *                 
*      LR    RE,R1                    Clear the area          *                 
*      LHI   RF,DwsSize               (same)                  *                 
*      XR    R1,R1                    (same)                  *                 
*      MVCL  RE,R0                    (same)                  *                 
*    ENDIF ,                        EndIf not yet acquired    *                 
*                                                             *                 
*  ********************************************************   *                 
*  * Perform Requested Function                           *   *                 
*  ********************************************************   *                 
*    L R1,CubicleTkn                Get cubicle token         *                 
*    SELECT CLI,InReqCode,EQ        Select InReqCode          *                 
*                                                             *                 
*    ************************************                     *                 
*    * Open the Cubicle                 *                     *                 
*    ************************************                     *                 
*      WHEN InReqOpen                 Open                    *                 
*        JAS  RE,OpenCubicle            Open the Cubicle      *                 
*        IF LTR,R2,RF,NZ                If Open failed        *                 
*          JAS  RE,CloseCubicle           Clean it up         *                 
*          LR RF,R2                       Restore retcode     *                 
*        ENDIF ,                        EndIf Open failed     *                 
*                                                             *                 
*    ************************************                     *                 
*    * Close the Cubicle                *                     *                 
*    ************************************                     *                 
*      WHEN InReqClose                Close                   *                 
*        JAS  RE,CloseCubicle           Close the Cubicle     *                 
*                                                             *                 
*    ************************************                     *                 
*    * Reformat the Cubicle             *                     *                 
*    ************************************                     *                 
*      WHEN InReqReformat             Reformat                *                 
*        JAS  RE,ReformatCubicle        Reformat the Cubicle  *                 
*                                                             *                 
*    ************************************                     *                 
*    * Refresh the Cubicle              *                     *                 
*    ************************************                     *                 
*      WHEN InReqrefresh              Refresh                 *                 
*        JAS  RE,ReformatCubicle        Refresh the Cubicle   *                 
*                                                             *                 
*    ************************************                     *                 
*    * Invalid Request Received         *                     *                 
*    ************************************                     *                 
*      OTHRWISE ,                     Otherwise               *                 
*        CUBSRV TYPE=SETMSG,            Set message data      *                 
*               MSGPARM=(LogicErr,      - Type = logic error  *                 
*                        InReqCode,     - Request code        *                 
*                        CubicleTkn)    - Cubicle token       *                 
*               THREAD=ThreadTkn        - Thread token        *                 
*        LHI RF,RetcodeError            Error return code     *                 
*                                                             *                 
*    ENDSEL ,                       EndSel InReqCode          *                 
*                                                             *                 
* RESTRICTIONS =                                              *                 
*                                                             *                 
*    1) This routine does not use any of its own facilities   *                 
*       for obvious reasons.                                  *                 
*                                                             *                 
*    2) It is not possible for this exit to detect certain    *                 
*       ambiguities. For example, it cannot distinguish       *                 
*       between a trailing comma in the operand field and     *                 
*       a trailing comma in the first word of the comment     *                 
*       if no operand is present. For example, the following  *                 
*       appears in IBM's IFGACBVS macro:                      *                 
*                                                             *                 
*       ____________________________________________________  *                 
*      |                                                    | *                 
*      |      AIF   ('&AM' NE 'VTAM').ENDACBX    IS IT VTAM | *                 
*      |         ISTACBEX          YES, GET VTAM EXTENSIONN | *                 
*      |.ENDACBX ANOP                                       | *                 
*      |____________________________________________________| *                 
*                                                             *                 
*       Therefore, we maintain a list of members for which no *                 
*       FLOWASM processing is to be performed. This list is   *                 
*       the FLOWERRM table.                                   *                 
*                                                             *                 
*       If you discover additional IBM or ISV macros that get *                 
*       assembly errors when processed by this exit, please   *                 
*       let the author (Ed Jaffe) know about them and, of     *                 
*       course, add them to FLOWERRM in your copy of this     *                 
*       exit routine.                                         *                 
*                                                             *                 
*    ======================================================== *                 
*                                                             *                 
*    Flow Bars Added by Listing Exit:                         *                 
*                                                             *                 
*    In order for the flow bars to work properly, the exit    *                 
*    must "see" the macro invocations that denote both the    *                 
*    start and end of the structure.                          *                 
*                                                             *                 
*    The following characters are used for "flow bars":       *                 
*                                                             *                 
*     º (split vertical) - indentation level 1, 4, 7, etc.    *                 
*     : (colon)          - indentation level 2, 5, 8, etc.    *                 
*     | (solid vertical) - indentation level 3, 6, 9, etc.    *                 
*                                                             *                 
*    If you prefer to use different characters, change the    *                 
*    constant called FLOWCHAR in this program.                *                 
*                                                             *                 
*END OF SPECIFICATIONS ****************************************                 
                                                                                
     SYSSTATE ARCHLVL=4                                                         
     DCBD  DSORG=PS,DEVD=DA         Define DCB DSECT                            
JFCB DSECT ,                        Define JFCB DSECT                           
     IEFJFCBN ,                     (same)                                      
**** DEIS SAYS: we don't need to COPY ASMMSP here, because we do that           
**** in our REQUS macro.                                                        
**** COPY  ASMMSP                   Enable Structured Programming               
     ASMMREL ON                     Enable relative branch for SPMs             
                                                                                
     ASMAXITP PRINT=GEN             Define exit parm lists                      
**** DEIS SAYS: we use REQUS instead of AMSDREG.                                
*****ASMDREG ,                      Define register equates                     
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                   Working Storage Mappings                  *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOWDWS  DSECT ,                   Dynamic Working Storage                      
                                                                                
         DS    18F                 Standard save area                           
FLOWSAV1 DS    16F                 Register save area                           
FLOWSAV2 DS    16F                 Register save area                           
FLOWSAV3 DS    16F                 Register save area                           
                                                                                
FLOWRECL DS    F    <-------Keep-\ Final record length                          
FLOWRECP DS    F  <-----Together-/ Final record pointer                         
                                                                                
FLOWOPCA DS    A                   OpCode Address                               
FLOWOPCZ DS    F                   OpCode Size                                  
                                                                                
FLOWOPRA DS    A                   Operand Address                              
FLOWOPRZ DS    F                   Operand Size                                 
                                                                                
FLOWCMTA DS    A                   Comment Address                              
FLOWCMTZ DS    F                   Comment Size                                 
                                                                                
FLOWMACW DS    XL256               Macro work area                              
                                                                                
FLOWDCB  DS    0D,XL(FLOWDCBML)    DCB area                                     
FLOWDCBX DS    0D,XL(FLOWDCBEL)    DCB extension                                
FLOWDCBL DS    4F                  DCB exit list                                
FLOWJFCB DS    XL(JFCBLGTH)        JFCB area                                    
                                                                                
FLOWBARS DS    0F,40XL(FLOWBarLen) Flow bar table                               
FLOWBARP DS    A  <--------------/ Pointer to newest flow bar entry             
                                                                                
FLOWOFFS DS    H                   Variable source record offset                
FLOWWKUC DS    H                   FLOWDWS usage count (OPEN count)             
FLOWSTMO DS    H                   Statement offset on listing                  
FLOWMCNO DS    H                   Assembler-provided macro name offset         
FLOWOPCO DS    H                   Opcode offset (in hex object code)           
                                                                                
FLOWPFL1 DS    XL1                 Permanent flags                              
FLOWP1ST  EQU   X'80'              - First record processed                     
FLOWPCON  EQU   X'40'              - Statement automatically continued          
FLOWPMAN  EQU   X'20'              - Statement manually continued               
FLOWPWRP  EQU   X'10'              - Statement should be wrapped                
FLOWPCMP  EQU   X'08'              - Compare instruction encountered            
*                                     prior to SPM-generated jump               
FLOWIGNR  EQU   X'01'              - Member should be ignored                   
FLOWTFL1 DS    XL1                 Temporary flags                              
FLOWTVAR  EQU   X'80'              - Variable length records                    
FLOWTEOD  EQU   X'40'              - End of data reached on input               
FLOWTREL  EQU   X'01'              - Release dynamic work area                  
FLOWOPTS DS    XL1                 Options                                      
FLOWOVAR  EQU   X'80'              - FLOWOPT RECFM=V                            
FLOWCCHR DS    CL1                 Continuation character                       
                                                                                
FLOWSTMW DS    CL256               Statement work area                          
         DS    CL256               Padding area for overflows                   
                                                                                
         DS    0D                  Align to doubleword boundary                 
FLOWDWSL EQU   *-FLOWDWS           Length of Dynamic Working Storage            
                                                                                
FLOWBar      DSECT ,               Flow bar entry                               
FLOWBarCol   DS    H               Column number                                
FLOWBarType  DS    XL1             Structure type                               
FLOWBarTyIf   EQU   1              ..If <-> EndIf                               
FLOWBarTyDo   EQU   2              ..Do <-> EndDo                               
FLOWBarTySel  EQU   3              ..Select <-> EndSel                          
FLOWBarTyCase EQU   4              ..CasEntry <-> EndCase                       
FLOWBarChar  DS    CL1             Character to be printed                      
FLOWBarLen   EQU   *-FLOWBar       Length of flow bar entry                     
                                                                                
WARNTABD DSECT ,                   Warning message table                        
MSGCODE  DS    CL8                 Error message code                           
AERRMSG  DS    A                   Additional message for listing               
ERRMSGL  DS    A                   L'Additional message                         
WARNTBLQ EQU   *-WARNTABD                                                       
                                                                                
CMPRINSD DSECT ,                   Compare instruction table                    
CMPRMNEL DS    AL1                 Mnemonic length                              
CMPRMNEM DS    CL6                 Mnemonic                                     
CMPRINLQ EQU   *-CMPRINSD                                                       
                                                                                
JUMPINSD DSECT ,                   Jump instruction table                       
JUMPOPCD DS    CL4                 Opcode (hex digits in EBCDIC format)         
JUMPJCMP DS    CL5                 Extended branch mnemonic after cmpr.         
JUMPJOTH DS    CL5                 Extended branch mnomonic after other         
JUMPTBLQ EQU   *-JUMPINSD                                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                     Program Entry Point                     *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOWASM  RSECT ,                                                                
     SAVE  (14,12),,*              Save the registers                           
     LR    RC,RF                   Load base register                           
     USING FLOWASM,RC          *** Synchronize base register                    
     J     FLOWCODE                Jump to code                                 
FLOWDATA LOCTR ,                   Define data LOCTR                            
FLOWCODE LOCTR ,                   Define code LOCTR                            
                                                                                
* Load Important Pointers                                                       
     LR    RB,R1                   Point to exit parm list                      
     USING AXPXITP,RB          *** Synchronize AXPXITP DSECT                    
     L     RA,AXPRIP               Load request information ptr                 
     USING AXPRIL,RA           *** Synchronize AXPRIL DSECT                     
     L     R9,AXPSIP               Load source information ptr                  
     USING AXPSIL,R9           *** Synchronize AXPSIL DSECT                     
                                                                                
* Load Common Storage Address                                                   
     IF (ICM,R1,B'1111',AXPUCOM,Z) If common area not acquired                  
       STORAGE OBTAIN,               Acquire the common area           +        
               LENGTH=FLOWDWSL,      (same)                            +        
               LOC=(24,64)           (same)                                     
       ST    R1,AXPUCOM              Save common area address                   
       LR    R0,R1                   Zero out the storage                       
       LA    R1,FLOWDWSL             (same)                                     
       XR    RF,RF                   (same)                                     
       MVCL  R0,RE                   (same)                                     
       JO    *+2                                                                
       L     R1,AXPUCOM              Load DWS address into R1                   
     ENDIF ,                       EndIf                                        
                                                                                
* Chain Save Areas                                                              
     ST    RD,4(,R1)               Chain save areas                             
     ST    R1,8(,RD)               (same)                                       
                                                                                
* Establish Dynamic Working Storage                                             
     LR    RD,R1                    Load DWS pointer                            
     USING FLOWDWS,RD          *** Synchronize FLOWDWS DSECT                    
     USING IHADCB,FLOWDCB      *** Synchronize IHADCB                           
     USING JFCB,FLOWJFCB       *** Synchronize JFCB                             
                                                                                
     MVC   AXPRETC,=F'0'           Set return code = 0                          
     MVC   AXPREAC,=F'0'           Set reason code = 0                          
     MVI   FLOWTFL1,X'00'          Zero temporary flags                         
                                                                                
***************************************************************                 
*                                                             *                 
*             Main-Line for Source/Library Exit               *                 
*                                                             *                 
***************************************************************                 
     IF (CLC,AXPTYPE,EQ,           If Source exit                      +        
               =A(AXPTSRC)),OR,    .. <OR>                             +        
               (CLC,AXPTYPE,EQ,    .. Library exit                     +        
               =A(AXPTLIB))        ..                                           
                                                                                
       SELECT CLC,AXPRTYP,EQ         Select Request Type                        
                                                                                
       WHEN (=A(AXPROPN))            When OPEN request                          
         JAS   RE,FLOW_SrcOpen         Open the file                            
         ST    RF,AXPRETC              Pass back return code                    
         ST    R0,AXPREAC              Pass back reason code                    
         LHI   R0,1                    Increment use count                      
         AH    R0,FLOWWKUC             (same)                                   
         STH   R0,FLOWWKUC             (same)                                   
                                                                                
       WHEN (=A(AXPRCLS))            When CLOSE request                         
         JAS   RE,FLOW_SrcClose        Close the file                           
         LH    R0,FLOWWKUC             Decrement use count                      
         SHI   R0,1                    (same)                                   
         IF (STH,R0,FLOWWKUC,NP)       If area no longer needed                 
           OI    FLOWTFL1,FLOWTREL       Show release of work area              
         ENDIF ,                       EndIf                                    
                                                                                
       WHEN (=A(AXPREAD))            When READ request                          
         JAS   RE,FLOW_SrcWrap         Get wrapped remainder                    
         IF (LTR,RF,RF,Z)              If wrapped remainder                     
           JAS   RE,FLOW_SrcCopy         Copy the statement                     
*                                         (Note: R0 and R1 are updated)         
         ELSE ,                        Else no wrapped remainder                
           JAS   RE,FLOW_SrcRead         Read a record                          
           ST    RF,AXPRETC              Pass back return code                  
           IF (LTR,RF,RF,Z)              If record available                    
             JAS   RE,FLOW_SrcProc         Process the statement                
             JAS   RE,FLOW_SrcCopy         Copy the statement                   
*                                         (Note: R0 and R1 are updated)         
           ENDIF ,                       EndIf record available                 
         ENDIF ,                       EndIf no wrapped remainder               
                                                                                
       WHEN (=A(AXPRPRO),            When PROCESS request              +        
               =A(AXPRPCPY))         (same)                                     
         L     R1,AXPBUFP              Get buffer address                       
         L     R0,AXPBUFL              Get statement length                     
         SHI   R0,8                    Adjust for seqnum area                   
         JAS   RE,FLOW_SrcProc         Process the statement                    
         JAS   RE,FLOW_SrcCopy         Copy statement (if needed)               
*                                       (Note: R0 and R1 are updated)           
                                                                                
       WHEN (=A(AXPRFMAC),           When FIND request                 +        
               =A(AXPRFCPY))         (same)                                     
         LA    R1,FLOWERRM             Point to error macro list                
         LHI   R0,FLOWERRM#            Get number of macros in list             
         DO FROM=(R0)                  Do for entire list                       
           IF (CLC,0(L'FLOWERRM,R1),EQ,  If matching macro name        +        
               AXPMEMN)                  ..                                     
             OI    FLOWPFL1,FLOWIGNR       Ignore this member                   
             ASMLEAVE ,                    Exit the loop                        
           ENDIF ,                       EndIf matching macro name              
           AHI   R1,L'FLOWERRM           Advance pointer                        
         ENDDO ,                       EndDo for entire list                    
         MVC   AXPRETC,=A(AXPCMNF)     Show member not found                    
                                                                                
       WHEN (=A(AXPREOM))            When END-OF-MEMBER notification            
         NI    FLOWPFL1,X'FF'-FLOWIGNR  Turn off member ignore flag             
                                                                                
       ENDSEL ,                       EndSel Request Type                       
                                                                                
     ENDIF ,                        EndIf Source or Library exit                
                                                                                
***************************************************************                 
*                                                             *                 
*                  Main-Line for Listing Exit                 *                 
*                                                             *                 
***************************************************************                 
     IF (CLC,AXPTYPE,EQ,=A(AXPTLST)) If Listing exit                            
                                                                                
       SELECT CLC,AXPRTYP,EQ         Select Request Type                        
                                                                                
       WHEN (=A(AXPROPN))            When OPEN request                          
         LHI   R0,1                    Increment use count                      
         AH    R0,FLOWWKUC             (same)                                   
         STH   R0,FLOWWKUC             (same)                                   
                                                                                
       WHEN (=A(AXPRCLS))            When CLOSE request                         
         LH    R0,FLOWWKUC             Decrement use count                      
         SHI   R0,1                    (same)                                   
         IF (STH,R0,FLOWWKUC,NP)       If area no longer needed                 
           OI    FLOWTFL1,FLOWTREL       Show release of work area              
         ENDIF ,                       EndIf                                    
                                                                                
       WHEN (=A(AXPRPRO))            When PROCESS request                       
         L     R1,AXPBUFP              Get buffer address                       
         L     R0,AXPBUFL              Get statement length                     
         DO ,                          Do for process                           
           JAS   RE,FLOW_LstGetStmtOff   Get source stmt offset                 
           DOEXIT (LTR,RF,RF,NP)         Exit if offset not known               
           ALR   R1,RF                   Point to source statement              
           DOEXIT (CLC,=C'.*',EQ,0(R1))  Don't process macro comments           
           LHI   R0,FLOWVSTL             Get source statement length            
           JAS   RE,FLOW_LstGetOpCol     Get opcode start column                
           IF (LTR,RF,RF,P)              If opcode found                        
             JAS   RE,FLOW_LstAddRemBars   Add/remove flow bars                 
             JAS   RE,FLOW_LstUseBRCMnemonics  Change BRC to Ext. Mnem.         
           ELSE ,                        Else                                   
             JAS   RE,FLOW_LstGetNonBlank Get first non-blank column            
           ENDIF ,                       EndIf opcode found                     
           JAS   RE,FLOW_LstFormatBars   Format the flow bars                   
         ENDDO ,                       EndDo for process                        
                                                                                
       ENDSEL ,                      EndSel Request Type                        
                                                                                
     ENDIF ,                       EndIf Listing exit                           
                                                                                
***************************************************************                 
*                                                             *                 
*                       Return Processing                     *                 
*                                                             *                 
***************************************************************                 
     LR    R2,RD                   Place DWS address into R2                    
     L     RD,4(,RD)               Load caller save area address                
     IF (TM,FLOWTFL1-FLOWDWS(R2),  If area to be released              +        
               FLOWTREL,O)         ..                                           
       STORAGE RELEASE,              Release Dynamic Work Area         +        
               ADDR=(2),             (same)                            +        
               LENGTH=FLOWDWSL       (same)                                     
       MVC   AXPUCOM,=F'0'           Zero out storage pointer                   
     ENDIF ,                       EndIf                                        
     RETURN (14,12),RC=0           Return to the assembler                      
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                       Source File OPEN                      *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcOpen DC 0H                                                              
     LHI   RF,AXPCOPL              Both HLASM & exit process lib                
     LHI   R0,AXPEEOM              Need END-OF-MEMBER calls                     
     CLC   AXPTYPE,=A(AXPTSRC)     Source input ?                               
     BNER  RE                      Branch if not                                
     STM   R1,RE,FLOWSAV1+4        Save registers 1-14                          
                                                                                
     MVC   FLOWDCB(FLOWDCBML),FLOWDCBM Copy DCB model                           
                                                                                
* DEIS: Get certain critical DCB attributes from the DCB pointer in the         
*       Exit parameter list (rather than relying on hard-coded DCB              
*       attributes within this program). In this way, we can handle             
*       assembler overrides (e.g. an alternate "SYSIN" ddname).                 
                                                                                
*       Note: DEIS observes that the RECFM attribute is *null* in the           
*       DCB pointed to by AXPDCBP. Additionally, the FLOWDCBM DCB does          
*       not specify a RECFM attribute. And yet, there is code in this           
*       exit which tests the contents of the DCBRECFM field for RECFM=U         
*       and RECFM=V. This leads DEIS to believe that there is a bug in          
*       this program as delivered, and the only reason we're getting            
*       away with it is because our source code is always RECFM=F (and          
*       we therefore always take the correct execution path for us).            
                                                                                
     L     R2,AXPDCBP              A(DCB pointer) from exit param. list         
DCB  USING IHADCB,R2                                                            
     MVC   DCBDDNAM,DCB.DCBDDNAM   Get the DDNAME from the passed DCB           
     MVC   DCBLRECL,DCB.DCBLRECL   Get the LRECL from the passed DCB            
     DROP  DCB                                                                  
                                                                                
     MVC   FLOWDCBX(FLOWDCBEL),FLOWDCBE Copy DCBE model                         
     LA    R0,FLOWDCBX             Set address of DCBE                          
     ST    R0,DCBDCBE              (same)                                       
     LA    R0,FLOWDCBL             Set address of exit list                     
     STCM  R0,B'0111',DCBEXLSA     (same)                                       
     LA    R0,FLOWJFCB             Set JFCB address in exit list                
     ICM   R0,B'1000',=X'07'       (same)                                       
     ST    R0,FLOWDCBL+00          (same)                                       
     OI    FLOWDCBL+00,X'80'       Indicate end of list                         
                                                                                
     DO ,                          Do for OPEN processing                       
       LA    R2,FLOWDCB              Point to the DCB                           
       MVC   FLOWMACW(FLOWRDFML),FLOWRDFM Copy model RDJFCB macro               
       RDJFCB ((2)),                 Read the JFCB                     +        
               MF=(E,FLOWMACW)       (same)                                     
       IF (LTR,RF,RF,NZ)             If non-zero return code                    
         LA    RF,AXPCBAD              Indicate OPEN failed                     
         XR    R0,R0                   Set reason code = 0                      
         ASMLEAVE ,                    Leave the structure                      
       ENDIF ,                       EndIf                                      
       IF (CLC,=C'SYS',EQ,JFCBDSNM)  If data set starts with 'SYS'              
         XR    RF,RF                   Set return code = 0                      
         XR    R0,R0                   Set reason code = 0                      
         ASMLEAVE ,                    Leave the structure                      
       ENDIF ,                       EndIf data set starts with 'SYS'           
                                                                                
       MVC   AXPDSN(44),JFCBDSNM     Copy data set name                         
       MVC   AXPMEMN(8),JFCBELNM     Copy member name                           
       MVC   AXPVOL(6),JFCBVOLS      Copy volume                                
       MVC   FLOWMACW(FLOW_SrcOpenML),FLOW_SrcOpenM Copy OPEN plist             
       OPEN  ((2),INPUT),            Open the DCB                      +        
               MODE=31,              (same)                            +        
               MF=(E,FLOWMACW)       (same)                                     
       IF (LTR,RF,RF,NZ)             If non-zero return code                    
         LA    RF,AXPCBAD              Indicate OPEN failed                     
         XR    R0,R0                   Set reason code = 0                      
         ASMLEAVE ,                    Leave the structure                      
       ENDIF ,                       EndIf                                      
       IF (TM,DCBRECFM,DCBRECU,O)    If RECFM=U                                 
         CLOSE ((2)),                  Close the DCB                   +        
               MODE=31,                (same)                          +        
               MF=(E,FLOWMACW)         (same)                                   
         LA    RF,AXPCBAD              Indicate OPEN failed                     
         XR    R0,R0                   Set reason code = 0                      
         ASMLEAVE ,                    Leave the structure                      
       ENDIF ,                       EndIf RECFM=U                              
       LA    RF,AXPCOPN              Indicate exit did the open                 
       LA    R0,AXPEDSA              Indicate data set info available           
     ENDDO ,                       EndDo for OPEN processing                    
                                                                                
* Return to Caller                                                              
     LM    R1,RE,FLOWSAV1+4        Restore registers 1-14                       
     BR    RE                      Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                       Source File CLOSE                     *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcClose DC 0H                                                             
     CLC   AXPTYPE,=A(AXPTSRC)     Source input ?                               
     BNER  RE                      Branch if not                                
     STM   R0,RF,FLOWSAV1          Save the registers                           
                                                                                
     IF (TM,DCBOFLGS,DCBOFOPN,O)   If DCB is open                               
       MVC   FLOWMACW(FLOW_SrcOpenML),FLOW_SrcOpenM Copy CLOSE plist            
       CLOSE (FLOWDCB),              Close the DCB                     +        
               MODE=31,              (same)                            +        
               MF=(E,FLOWMACW)       (same)                                     
     ENDIF ,                       EndIf DCB is open                            
                                                                                
* Return to Caller                                                              
     LM    R0,RF,FLOWSAV1          Restore the registers                        
     BR    RE                      Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*            Get Wrapped Remainder of Source Record           *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcWrap DC 0H                                                              
     LHI   RF,4                    Set return code = 4                          
     TM    FLOWPFL1,FLOWPWRP       Any wrapped record ?                         
     BZR   RE                      Return if not                                
                                                                                
     STM   R2,RE,FLOWSAV1+8        Save registers 2-14                          
                                                                                
     NI    FLOWPFL1,X'FF'-FLOWPWRP Turn off wrapping flag                       
     LM    R0,R1,FLOWRECL          Get internal rec len & ptr                   
     A     R1,AXPBUFL              Advance pointer                              
     SHI   R1,8+CONTINUATION_START_COLUMN (same)                                
     AHI   R0,8+CONTINUATION_START_COLUMN Adjust remaining length               
     IF (S,R0,AXPBUFL,NP)          If nothing left to wrap                      
       LHI   RF,4                    Set return code = 4                        
     ELSE ,                        Else                                         
       MVC   0(15,R1),=CL15' '       Set 15 leading blanks                      
       IF (CLC,FLOWOFFS,NE,=H'0')    If sequence numbers                        
         L     RE,FLOWRECP             Point to seqnum (if any)                 
         SH    RE,FLOWOFFS             (same)                                   
         LR    RF,R1                   Point where seqnum goes                  
         SHI   RF,8                    (same)                                   
         MVC   0(8,RF),0(RE)           Copy sequence number                     
       ENDIF ,                       EndIf sequence numbers                     
       XR    RF,RF                   Set return code = 0                        
     ENDIF ,                       EndIf nothing left to wrap                   
                                                                                
* Return to Caller                                                              
     LM    R2,RE,FLOWSAV1+8        Restore registers 2-14                       
     BR    RE                      Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                     Source/Library READ                     *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcRead DC 0H                                                              
     STM   R2,RE,FLOWSAV1+8        Save registers 2-14                          
                                                                                
     GET   FLOWDCB                 Get a record (locate mode)                   
     LR    R3,R1                   Save record address                          
                                                                                
* Handle *FLOWOPT Statement                                                     
     IF (TM,FLOWTFL1,FLOWTEOD,Z)   If record available                          
       IF (TM,FLOWPFL1,FLOWP1ST,Z)   If first record                            
         XR    R0,R0                   Zero search length                       
         IF (CLC,=CL9'*FLOWOPT ',EQ,   If *FLOWOPT in pos 1            +        
               0(R1))                  ..                                       
           LA    R1,9(,R1)               Point past *FLOWOPT                    
           IF (TM,DCBRECFM,DCBRECV,O)    If variable format                     
             LH    R0,0(,R3)               Get length of record                 
             SHI   R0,(4-9)                Adjust for RDW & *FLOWOPT            
           ELSE ,                        Else must be fixed format              
             LH    R0,DCBLRECL             Get length of record                 
             SHI   R0,9                    Adjust for *FLOWOPT                  
           ENDIF ,                       EndIf variable format                  
         ENDIF ,                       EndIf *FLOWOPT in pos 1                  
         IF (CLC,=CL9'*FLOWOPT ',EQ,   If *FLOWOPT in pos 9            +        
               8(R1))                  ..                                       
           LA    R1,8+9(,R1)             Point past *FLOWOPT                    
           IF (TM,DCBRECFM,DCBRECV,O)    If variable format                     
             LH    R0,0(,R3)               Get length of record                 
             SHI   R0,(4-8-9)              Adjust for RDW, seq, & HOPTS         
           ELSE ,                        Else must be fixed format              
             LH    R0,DCBLRECL             Get length of record                 
             SHI   R0,(8-9)                Adjust for seqnum & *FLOWOPT         
           ENDIF ,                       EndIf variable format                  
         ENDIF ,                       EndIf *FLOWOPT in pos 9                  
         IF (LTR,R0,R0,NZ)             If *FLOWOPT present                      
           DO FROM=(R0)                  Do for entire record                   
             IF (CHI,R0,GE,7)              If remaining len >= 7                
               IF (CLC,=C'RECFM=V',EQ,0(R1)) If RECFM=V specified               
                 OI    FLOWOPTS,FLOWOVAR       Show variable source             
               ENDIF ,                       EndIf                              
             ENDIF ,                       EndIf                                
             AHI   R1,1                    Increment pointer                    
           ENDDO ,                       EndDo for entire record                
           GET   FLOWDCB                 Get a record (locate mode)             
           LR    R3,R1                   Save record address                    
         ENDIF ,                       EndIf *FLOWOPT present                   
       ENDIF ,                       EndIf first record                         
     ENDIF ,                       EndIf record available                       
                                                                                
     IF (TM,FLOWTFL1,FLOWTEOD,Z)   If record available                          
                                                                                
*   Handle Variable Length Records                                              
       IF (TM,DCBRECFM,DCBRECV,O)    If variable format                         
         OI    FLOWTFL1,FLOWTVAR       Show variable length records             
                                                                                
*     Check if Records are Numbered                                             
         IF (TM,FLOWPFL1,FLOWP1ST,Z)   If first record                          
           MVC   FLOWMACW(8),4(R3)       Copy possible seqnum                   
           OC    FLOWMACW(8),=8C'0'      OR with X'F0' characters               
           IF (CLC,FLOWMACW(8),EQ,4(R3)) If no change (numbered)                
             MVC   FLOWOFFS,=H'8'          Set offset past seqnum               
           ENDIF ,                       EndIf                                  
         ENDIF ,                       EndIf first record                       
                                                                                
*     Load Record Pointers                                                      
         LA    R1,4(,R3)               Point to start of record                 
         AH    R1,FLOWOFFS             Adjust for seqnum                        
         LH    R0,0(,R3)               Get length of record                     
         SHI   R0,4                    Subtract length of RDW                   
         SH    R0,FLOWOFFS             Adjust for seqnum                        
                                                                                
*   Handle Fixed Length Records                                                 
       ELSE ,                        Else must be fixed format                  
                                                                                
         IF (TM,FLOWOPTS,FLOWOVAR,O)   If FLOWOPTS RECFM=V                      
           OI    FLOWTFL1,FLOWTVAR       Treat like variable length             
                                                                                
*       Check if Records are Numbered                                           
           IF (TM,FLOWPFL1,FLOWP1ST,Z)   If first record                        
             MVC   FLOWMACW(8),0(R3)       Copy possible seqnum                 
             OC    FLOWMACW(8),=8C'0'      OR with X'F0' characters             
             IF (CLC,FLOWMACW(8),EQ,0(R3)) If no change (numbered)              
               MVC   FLOWOFFS,=H'8'          Set offset past seqnum             
             ENDIF ,                       EndIf                                
           ENDIF ,                       EndIf first record                     
                                                                                
*       Trim Trailing Blanks                                                    
           LH    R0,DCBLRECL             Get length of record                   
           LR    R1,R3                   Point to end of record                 
           AR    R1,R0                   (same)                                 
           SHI   R1,1                    (same)                                 
           SH    R0,FLOWOFFS             Adjust length for seqnum               
           DO FROM=(R0)                  Do for trailing blanks                 
             DOEXIT (CLI,0(R1),GT,C' ')    Exit loop if non-blank               
             SHI   R1,1                    Decrement pointer                    
           ENDDO ,                       EndDo for trailing blanks              
                                                                                
*       Load Record Pointer                                                     
           LR    R1,R3                   Point to start of record               
           AH    R1,FLOWOFFS             Adjust for seqnum                      
                                                                                
         ELSE ,                        Else truly fixed length                  
           LR    R1,R3                   Point to start of record               
           LH    R0,DCBLRECL             Get length of record                   
           SHI   R0,8                    Adjust for seqnum                      
         ENDIF ,                       EndIf FLOWOPTS RECFM=V                   
                                                                                
       ENDIF ,                       EndIf variable format                      
                                                                                
       OI    FLOWPFL1,FLOWP1ST       Show first record processed                
       XR    RF,RF                   Set return code = 0                        
     ELSE ,                        Else end of data reached                     
       LA    RF,AXPCEOD              Indicate end of data                       
     ENDIF ,                       EndIf record available                       
                                                                                
* Return to Caller                                                              
     LM    R2,RE,FLOWSAV1+8        Restore registers 2-14                       
     BR    RE                      Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                      End of Data Exit                       *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcEod DC 0H                                                               
     OI    FLOWTFL1,FLOWTEOD       Indicate end of data                         
     BR    RE                      Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                   Process Source Statement                  *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcProc DC 0H                                                              
     STM   R0,RF,FLOWSAV1          Save the registers                           
                                                                                
     MVI   FLOWCCHR,C' '           Blank continuation char                      
     IF (TM,FLOWPFL1,FLOWPMAN,O)   If manually continued                        
       NI    FLOWPFL1,X'FF'-FLOWPMAN Turn off manual cont. flg                  
       JAS   RE,FLOW_SrcManualCont   Check for manual cont.                     
     ELSE ,                        Else                                         
       IF (TM,FLOWPFL1,FLOWPCON,O)   If continued statement                     
         NI    FLOWPFL1,X'FF'-FLOWPCON Turn off continue flag                   
         JAS   RE,FLOW_SrcManualCont   Check for manual cont.                   
         JAS   RE,FLOW_SrcContStmt     Process continuation                     
*                                       (Note: R0 and R1 are updated)           
       ELSE ,                        Else                                       
         JAS   RE,FLOW_SrcManualCont   Check for manual cont.                   
         IF (TM,FLOWPFL1,FLOWPMAN,Z)   If not manual cont.                      
           IF (TM,FLOWPFL1,FLOWIGNR,Z)   If not ignored                         
             JAS   RE,FLOW_SrcNewStmt      Process new statement                
           ENDIF ,                       EndIf                                  
         ENDIF ,                       EndIf not manual cont.                   
       ENDIF ,                       EndIf continued statement                  
     ENDIF ,                       EndIf manually continued                     
                                                                                
* Return to Caller                                                              
     LM    R2,RF,FLOWSAV1+8        Restore registers 2-15                       
     BR    RE                      Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                     Copy Source Statement                   *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcCopy DC 0H                                                              
     STM   R0,RF,FLOWSAV1          Save the registers                           
                                                                                
     STM   R0,R1,FLOWRECL          Save internal rec len & ptr                  
     IF (C,R1,NE,AXPBUFP)          If input != output                           
       L     R2,AXPBUFP              Get buffer address                         
       L     R3,AXPBUFL              Get buffer length                          
       SHI   R3,8                    Adjust for seqnum                          
       LR    RE,R1                   Point to start of record                   
       LR    RF,R0                   Get length of record                       
       ICM   RF,B'1000',=C' '        Set blank pad character                    
       SHI   R3,1                    Adjust for continuation                    
       MVCL  R2,RE                   Copy record to buffer                      
       JO    *+2                                                                
       IF (TM,FLOWTFL1,FLOWTVAR,O)   If variable length                         
         IF (CLC,FLOWOFFS,NE,=H'0')    If numbered records                      
           SH    R1,FLOWOFFS             Point to seqnum                        
           MVC   1(8,R2),0(R1)           Copy seqnum                            
         ELSE ,                        Else ,                                   
           MVC   1(8,R2),=CL8' '         Make seqnum blanks                     
         ENDIF ,                       EndIf numbered records                   
       ELSE ,                        Else fixed length                          
         AH    R1,DCBLRECL             Point to seqnum                          
         SHI   R1,8                    (same)                                   
* DEIS: See Mar/2018 comment in FLOW_SrcContStmt routine. Prior to that         
*       change, the seqnum incorrectly contained garbage at this point.         
         MVC   1(8,R2),0(R1)           Copy seqnum                              
       ENDIF ,                       EndIf variable length                      
     ENDIF ,                       EndIf input != output                        
                                                                                
     L     R1,AXPBUFP              Get buffer address                           
     L     R0,AXPBUFL              Get buffer length                            
     SHI   R0,8                    Adjust for seqnum                            
* Watch out! R0 and R1 are being written back to the save area here,            
* and then further down, *all* of the registers are being restored from         
* the save area via an LM instruction. I.e., when we return from this           
* routine, we essentially only restore the caller's R2..RF, and we              
* return updated values in R0 and R1.                                           
     STM   R0,R1,FLOWSAV1          Pass back new length & ptr                   
     AR    R1,R0                   Point to where cchr goes                     
     SHI   R1,1                    (same)                                       
     MVC   0(1,R1),FLOWCCHR        Copy continuation char                       
     IF (TM,FLOWPFL1,FLOWPWRP,O)   If record should be wrapped                  
       MVI   0(R1),C'-'              Force continuation char                    
     ENDIF ,                       EndIf record should be wrapped               
                                                                                
* Return to Caller                                                              
     LM    R0,RF,FLOWSAV1          Restore the registers                        
     BR    RE                      Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                 Check for Manual Continuation               *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcManualCont DC 0H                                                        
     STM   R0,RF,FLOWSAV2          Save the registers                           
                                                                                
* Check for Continuation                                                        
     AR    R1,R0                   Point to last character                      
     SHI   R1,1                    (same)                                       
     IF (TM,FLOWTFL1,FLOWTVAR,O)   If variable length                           
       IF (CLI,0(R1),EQ,C'+')        If statement manually continued            
         OI    FLOWPFL1,FLOWPMAN       Show manually continued                  
         MVC   FLOWCCHR,0(R1)          Copy user's cont character               
         MVI   0(R1),C' '              Blank out user's character               
       ENDIF ,                       EndIf                                      
     ELSE ,                        Else fixed length                            
       IF (CLI,0(R1),NE,C' ')        If statement manually continued            
         OI    FLOWPFL1,FLOWPMAN       Show manually continued                  
         MVC   FLOWCCHR,0(R1)          Copy user's cont character               
         MVI   0(R1),C' '              Blank out user's character               
       ENDIF ,                       EndIf                                      
     ENDIF ,                       EndIf variable length                        
                                                                                
* Trim Trailing Blanks                                                          
     DO FROM=(R0)                  Do for trailing blanks                       
       DOEXIT (CLI,0(R1),GT,C' ')    Exit loop if non-blank                     
       SHI   R1,1                    Decrement pointer                          
     ENDDO ,                       EndDo for trailing blanks                    
                                                                                
* Return to Caller                                                              
     LM    R1,RF,FLOWSAV2+4        Restore registers 1-15                       
     BR    RE                      Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                 Process Continued Statement                 *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcContStmt DC 0H                                                          
     STM   R0,RF,FLOWSAV2          Save the registers                           
                                                                                
     LR    R3,R1                   Load statement address                       
     LR    R4,R0                   Get statement length                         
     MVC   FLOWOPRA,=F'0'          Zero out Operand address                     
     MVC   FLOWOPRZ,=F'0'          Zero out Operand size                        
     MVC   FLOWCMTA,=F'0'          Zero out Comment address                     
     MVC   FLOWCMTZ,=F'0'          Zero out Comment size                        
                                                                                
* Parse Continued Statement                                                     
     IF (CLI,0(R3),GT,C' ')        If label present                             
       L     RE,AXPERRP              Load error buffer address                  
       MVC   0(L'FLOWERR1,RE),FLOWERR1 Set error message text                   
       MVC   AXPERRL,=A(L'FLOWERR1)  Set error message length                   
       MVC   AXPSEVC,=F'4'           Set error message severity                 
     ELSE ,                        Else                                         
       DO ,                          Do for continued statement                 
         JAS   RE,FLOW_SrcAdvWhite     Find the Operand                         
         DOEXIT (LTR,R4,R4,NP)         Exit if no more data                     
         ST    R3,FLOWOPRA             Save Operand address                     
         JAS   RE,FLOW_SrcParse        Advance past Operand                     
         ST    R1,FLOWOPRZ             Save Operand size                        
         DOEXIT (LTR,R4,R4,NP)         Exit if no more data                     
         JAS   RE,FLOW_SrcAdvWhite     Find the Comment                         
         DOEXIT (LTR,R4,R4,NP)         Exit if no more data                     
         ST    R3,FLOWCMTA             Save Comment address                     
         L     R3,FLOWSAV2             Get stmt length from saved R0            
         A     R3,FLOWSAV2+4           Add A(statement) from saved R1           
         SHI   R3,1                    R3 = A(last byte of statement)           
         DO WHILE=(CLI,0(R3),LE,C' ')  Do while trailing blank                  
           BCTR  R3,0                    Decrement pointer                      
         ENDDO ,                       EndDo                                    
         AHI   R3,1                    Compute size of Comment                  
         S     R3,FLOWCMTA             (same)                                   
         ST    R3,FLOWCMTZ             Save Comment size                        
       ENDDO ,                       EndDo for continued statement              
     ENDIF ,                       EndIf label present                          
                                                                                
* Supply Continuation if Operand Requires it                                    
     IF (ICM,RE,B'1111',FLOWOPRA,NZ) If Operand exists                          
       IF (CLI,0(RE),NE,C',')        If 1st byte not a comma                    
         A     RE,FLOWOPRZ             Point to last Operand byte               
         SHI   RE,1                    (same)                                   
         IF (CLI,0(RE),EQ,C',')        If last byte is a comma                  
           MVI   FLOWCCHR,C'-'           Set continuation char                  
           OI    FLOWPFL1,FLOWPCON       Set continuation flag                  
         ENDIF ,                       EndIf                                    
       ENDIF ,                       EndIf                                      
     ENDIF ,                       EndIf                                        
                                                                                
* Make the Continuation Statement Valid                                         
     IF (ICM,RE,B'1111',FLOWOPRA,NZ) If Operand exists                          
*                                    FLOWSAV2+4 = A(statement)                  
       S     RE,FLOWSAV2+4           Compute Operand offset                     
                                                                                
*   Move Operand if Necessary                                                   
*                                    If Operand in wrong location               
       IF (CHI,RE,NE,CONTINUATION_START_COLUMN-1)                               
         MVI   FLOWSTMW,C' '           Blank out statement work area            
         MVC   FLOWSTMW+1(L'FLOWSTMW-1),FLOWSTMW              (same)            
                                                                                
         IF (CLC,AXPRTYP,EQ,           If library input (SYSLIB)       +        
               =A(AXPRPRO)),AND,       .. <AND>                        +        
               (CLC,FLOWOPRZ,GT,       .. Operand size > available     +        
               =A(FLOWVSTL-CONTINUATION_START_COLUMN)) .. area size.            
           L     RE,AXPERRP              Load error buffer address              
           MVC   0(L'FLOWERR2,RE),FLOWERR2 Set error message text               
           MVC   AXPERRL,=A(L'FLOWERR2)  Set error message length               
           MVC   AXPSEVC,=F'8'           Set error message severity             
         ELSE ,                        Else wrapping allowed                    
           L     RE,FLOWOPRA             Point to source location               
           L     RF,FLOWOPRZ             Get source length                      
*                                        Point to target location               
           LA    R0,FLOWSTMW+(CONTINUATION_START_COLUMN-1)                      
           LR    R1,RF                   Get target length                      
           MVCL  R0,RE                   Move the Operand                       
           JO    *+2                                                            
                                                                                
*       Move Comment if Necessary                                               
           IF (CLC,FLOWCMTA,NE,=A(0))    If Comment exists                      
*                                                                               
* Remove Annoying Error Message                                                 
*           LA    R1,FLOWVSTL-(16+1)       Compute length available             
*           S     R1,FLOWOPRZ              (same)                               
*           IF C,R1,LT,FLOWCMTZ            If Comment too large                 
*             L     RE,AXPERRP               Load error buffer address          
*             MVC   0(L'FLOWERR3,RE),FLOWERR3 Set error message text            
*             MVC   AXPERRL,=A(L'FLOWERR3)   Set error message length           
*             MVC   AXPSEVC,=F'0'            Set error message severity         
*           ENDIF ,                        EndIf                                
*                                                                               
*                                          Get first possible Comment           
             LA    R1,CONTINUATION_START_COLUMN                                 
             A     R1,FLOWOPRZ               offset (Operand + 1).              
             L     R0,FLOWCMTA             Compute existing offset              
*                                          Subtract saved R1...                 
             S     R0,FLOWSAV2+4           ...which is A(stmt)                  
             IF (CR,R0,GT,R1)              If existing offset OK                
               LR    R1,R0                   Use existing offset                
             ENDIF ,                       EndIf                                
             LA    R0,FLOWSTMW(R1)         Point to target address              
             L     R1,FLOWCMTZ             Get target length                    
             L     RE,FLOWCMTA             Point to source location             
             LR    RF,R1                   Get source length                    
             MVCL  R0,RE                   Move the Comment                     
             JO    *+2                                                          
           ENDIF ,                       EndIf                                  
                                                                                
           LA    R1,FLOWSTMW             Point to work area                     
*================== REMOVED BY DEIS MAR/2018 =========================*         
*          SR    R0,R1                   Compute length of data                 
*=====================================================================*         
* In Mar/2018, DEIS found a bug, viz.: when an automatically-continued          
* statement was reformatted to begin in column 16, FLOWASM overwrote            
* the 8-byte sequence number area in the statement with garbage. That           
* wasn't good, because in our shop, that field contains not only the            
* Panvalet line number, but the level number as well. When that field           
* is corrupted, it not only affects the listing, but the ADATA file as          
* well. As a result, the sequence number field is displayed as garbage          
* in IDF. To fix this, code has been added below which assumes that the         
* 8-byte sequence number field can (and should) always be retained from         
* from the *last* 8 bytes of the original input record, regardless of           
* how that record is reformatted for continuation.                              
*                                                                               
           LH    RF,DCBLRECL             (probably always 80 for us)            
           SHI   RF,8                    L'sequence number                      
           L     RE,FLOWSAV2+4           RE = A(orig. unmodified stmt)          
           AR    RE,RF                   RE = A(original seqnum)                
           LA    RF,FLOWSTMW(RF)         RF = A(new seqnum)                     
           MVC   0(8,RF),0(RE)           copy seqnum to work area               
           LH    R0,DCBLRECL             always process the entire rec.         
*                                                                               
* Watch out! R0 and R1 are being written back to the save area here,            
* and then further down, *all* of the registers are being restored from         
* the save area via an LM instruction. I.e., when we return from this           
* routine, we essentially only restore the caller's R2..RF, and we              
* return updated values in R0 and R1.                                           
           STM   R0,R1,FLOWSAV2          Pass back new length & ptr             
         ENDIF ,                       EndIf SYSLIB & Operand too big           
       ENDIF ,                       EndIf Operand in wrong location            
                                                                                
       IF (CLC,FLOWOPRZ,GT,          If operand will wrap              +        
               =A(FLOWVSTL-(CONTINUATION_START_COLUMN-1))) (same)               
         OI    FLOWPFL1,FLOWPWRP       Request wrapping of operand              
       ENDIF ,                       EndIf operand will wrap                    
                                                                                
     ENDIF ,                       EndIf Operand exists                         
                                                                                
* Return to Caller                                                              
     LM    R0,RF,FLOWSAV2          Restore the registers                        
     BR    RE                      Return to caller                             
                                                                                
***************************************************************                 
*                                                             *                 
*                     Process New Statement                   *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcNewStmt DC 0H                                                           
     STM   R0,RF,FLOWSAV2          Save the registers                           
     LR    R3,R1                   Load statement address                       
     LR    R4,R0                   Get statement length                         
     MVC   FLOWOPCA,=F'0'          Zero out OpCode address                      
     MVC   FLOWOPCZ,=F'0'          Zero out OpCode size                         
     MVC   FLOWOPRA,=F'0'          Zero out Operand address                     
     MVC   FLOWOPRZ,=F'0'          Zero out Operand size                        
                                                                                
* Locate First Non-Blank Character                                              
     LR    RE,R3                   Save current address                         
     IF (LTR,R0,R4,P)              Save remaining length (if any)               
       DO FROM=(R0)                  Do for chars remaining                     
         DOEXIT (CLI,0(RE),GT,C' ')    Exit if non-blank found                  
         LA    RE,1(,RE)               Advance pointer                          
       ENDDO ,                       EndDo for chars remaining                  
     ENDIF ,                       EndIf something left                         
                                                                                
* Process Unaligned Comments                                                    
     IF (LTR,R0,R0,NZ)             If entire line not blank                     
       IF (CLI,0(RE),EQ,C'*')        If a leading asterisk                      
         MVI   0(R3),C'*'              Set asterisk in column 1                 
       ELSE ,                        Else ..                                    
         IF (CLC,=C'/*',EQ,0(RE))      If a leading '/*'                        
           MVI   0(R3),C'*'              Set asterisk in column 1               
         ENDIF ,                       EndIf                                    
       ENDIF ,                       EndIf                                      
     ENDIF ,                       EndIf                                        
                                                                                
     IF (CLI,0(R3),NE,C'*'),AND,   If not a comment AND                +        
               (CLI,0(R3),NE,C'.') .. not a macro statement                     
                                                                                
*   Parse the Statement                                                         
       IF (CLI,0(R3),GT,C' ')        If label present                           
         JAS   RE,FLOW_SrcParse        Advance past Label                       
       ENDIF ,                       EndIf                                      
       DO ,                          Do for parse                               
         DOEXIT (LTR,R4,R4,NP)         Exit if no more data                     
         JAS   RE,FLOW_SrcAdvWhite     Find the OpCode                          
         DOEXIT (LTR,R4,R4,NP)         Exit if no more data                     
         ST    R3,FLOWOPCA             Save OpCode address                      
         JAS   RE,FLOW_SrcParse        Advance past OpCode                      
         ST    R1,FLOWOPCZ             Save OpCode size                         
         DOEXIT (LTR,R4,R4,NP)         Exit if no more data                     
         JAS   RE,FLOW_SrcAdvWhite     Find the Operand                         
         DOEXIT (LTR,R4,R4,NP)         Exit if no more data                     
         ST    R3,FLOWOPRA             Save Operand address                     
         JAS   RE,FLOW_SrcParse        Advance past Operand                     
         ST    R1,FLOWOPRZ             Save Operand size                        
       ENDDO ,                       EndDo for parse                            
                                                                                
*   For safety, check the OpCode against a list of macros which are             
*   eligible for automatic continuation. This protects us from legacy           
*   code with extraneous commas in a macro parameter list which would           
*   cause an unwanted automatic continuation.                                   
       L     RF,FLOWOPCA             A(OpCode)                                  
       L     R1,FLOWOPCZ             Length of OpCode token                     
       LA    RE,FLOWDDSM             Macros eligible for continuation           
       DO WHILE=(CLI,0(RE),NE,X'FF') Do while not EOT                           
         EXRL  R1,*+10                 Compare on OpCode length, plus..         
         J     *+10                    ..one for a blank delimiter              
         CLC   0(0,RE),0(RF)           Macro is in the table?                   
         IF (EQ)                       If so:                                   
*   Supply Continuation if Operand Requires it                                  
           IF (ICM,R3,B'1111',FLOWOPRA,NZ) If Operand exists                    
             IF (CLI,0(R3),NE,C',')        If 1st byte not a comma              
               A     R3,FLOWOPRZ             Point to last Operand byte         
               BCTR  R3,0                    (same)                             
               IF (CLI,0(R3),EQ,C',')        If last byte is a comma            
                 MVI   FLOWCCHR,C'-'           Set continuation char            
                 OI    FLOWPFL1,FLOWPCON       Set continuation flag            
               ENDIF ,                       EndIf                              
             ENDIF ,                       EndIf                                
           ENDIF ,                       EndIf                                  
         DOEXIT ,                                                               
         ENDIF ,                                                                
         LA    RE,L'FLOWDDSM(,RE)        Bump to next macro in table            
       ENDDO ,                                                                  
                                                                                
*   Remove Pre-Comment Blanks for Long Statements                               
       LM    R0,R1,FLOWSAV2          Get statement length & ptr                 
       IF (CHI,R0,GT,FLOWVSTL)       If statement too long                      
         IF (ICM,RE,B'1111',FLOWOPRA,NZ) If Operand exists                      
           SR    RE,R1                   Get offset to operand                  
           AR    R1,RE                   Point past operand                     
           A     R1,FLOWOPRZ             (same)                                 
           S     R0,FLOWOPRZ             Compute length remaining               
           SR    R0,RE                   (same)                                 
           AHI   R1,1                    Advance past blank                     
           SHI   R0,1                    Decrement length remaining             
           IF (P),AND,(CLI,0(R1),LE,C' ') If two blanks in a row                
             LR    R2,R1                   Point to current blank               
             LR    R3,R0                   Get length remaining                 
             DO FROM=(R3)                  Do for extra blanks                  
               DOEXIT (CLI,0(R2),GT,C' ')    Exit if non-blank found            
               AHI   R2,1                    Advance pointer                    
             ENDDO ,                       EndDo for extra blanks               
             LR    RE,R1                   Get target address                   
             LR    RF,R3                   Get target length                    
             SR    R0,R3                   Compute # of squeezed blanks         
             MVCL  RE,R2                   Squeeze out the blanks               
             JO    *+2                                                          
             LNR   R0,R0                   Adjust total length                  
             A     R0,FLOWSAV2             (same)                               
             ST    R0,FLOWSAV2             (same)                               
           ENDIF ,                       EndIf two blanks in a row              
         ENDIF ,                       EndIf Operand exists                     
       ENDIF ,                       EndIf statement too long                   
                                                                                
*   Remove Pre-Operand Blanks for Long Statements                               
       LM    R0,R1,FLOWSAV2          Get statement length & ptr                 
       IF (CHI,R0,GT,FLOWVSTL)       If statement too long                      
         IF (ICM,R0,B'1111',FLOWOPRA,NZ) If Operand exists                      
           SR    R0,R1                   Compute bytes before operand           
           LR    RE,R0                   Save value in RE                       
           L     R1,FLOWOPRA             Point to byte before operand           
           SHI   R1,1                    (same)                                 
           DO FROM=(R0)                  Do for all blanks                      
             DOEXIT (CLI,0(R1),GT,C' ')    Exit if non-blank                    
             SHI   R1,1                    Decrement pointer                    
           ENDDO ,                       EndDo for all blanks                   
           SR    RE,R0                   Compute blanks between terms           
           SHI   RE,1                    Adjust for necessary blank             
           IF (P)                        If something to squeeze                
             LR    R0,RE                   Save squeeze count                   
             LA    RE,2(,R1)               Get target address                   
             L     R2,FLOWOPRA             Get source address                   
             LR    R1,R2                   Compute bytes before Operand         
*                                          Subtract saved R1...                 
             S     R1,FLOWSAV2+4           ...which is A(stmt)                  
             L     R3,FLOWSAV2             Get total length                     
             SR    R3,R1                   Compute source length                
             LR    RF,R3                   Target length = source               
             MVCL  RE,R2                   Squeeze out the blanks               
             JO    *+2                                                          
             L     RE,FLOWOPRA             Adjust operand address               
             SR    RE,R0                   (same)                               
             ST    RE,FLOWOPRA             (same)                               
             LNR   R0,R0                   Adjust total length                  
             A     R0,FLOWSAV2             (same)                               
             ST    R0,FLOWSAV2             (same)                               
           ENDIF ,                       If something to squeeze                
         ENDIF ,                       EndIf Operand exists                     
       ENDIF ,                       EndIf statement too long                   
                                                                                
*   Remove Pre-Opcode Blanks for Long Statements                                
       LM    R0,R1,FLOWSAV2          Get statement length & ptr                 
       IF (CHI,R0,GT,FLOWVSTL)       If statement too long                      
         DO FROM=(R0)                  Do for non-blanks                        
           DOEXIT (CLI,0(R1),LE,C' ')    Exit if blank found                    
           AHI   R1,1                    Increment pointer                      
         ENDDO ,                       EndDo for non-blanks                     
         AHI   R1,1                    Advance past blank                       
         SHI   R0,1                    Decrement length remaining               
         IF (P),AND,(CLI,0(R1),LE,C' ') If two blanks in a row                  
           LR    R2,R1                   Point to current blank                 
           LR    R3,R0                   Get length remaining                   
           DO FROM=(R3)                  Do for extra blanks                    
             DOEXIT (CLI,0(R2),GT,C' ')    Exit if non-blank found              
             AHI   R2,1                    Advance pointer                      
           ENDDO ,                       EndDo for extra blanks                 
           LR    RE,R1                   Get target address                     
           LR    RF,R3                   Get target length                      
           SR    R0,R3                   Compute # of squeezed blanks           
           MVCL  RE,R2                   Squeeze out the blanks                 
           JO    *+2                                                            
           L     RE,FLOWOPRA             Adjust operand address                 
           SR    RE,R0                   (same)                                 
           ST    RE,FLOWOPRA             (same)                                 
           LNR   R0,R0                   Adjust total length                    
           A     R0,FLOWSAV2             (same)                                 
           ST    R0,FLOWSAV2             (same)                                 
         ENDIF ,                       EndIf two blanks in a row                
       ENDIF ,                       EndIf statement too long                   
                                                                                
       IF (ICM,RE,B'1111',FLOWOPRA,NZ) If Operand exists                        
         A     RE,FLOWOPRZ             Point past Operand                       
*                                      Subtract saved R1...                     
         S     RE,FLOWSAV2+4           ...which is A(stmt)                      
*                                      RE now has offset past operand           
         IF (CHI,RE,GT,FLOWVSTL)       If operand will wrap                     
           OI    FLOWPFL1,FLOWPWRP       Request wrapping of operand            
         ENDIF ,                       EndIf operand will wrap                  
       ENDIF ,                       EndIf Operand exists                       
                                                                                
     ENDIF ,                       EndIf not comment & not macro                
                                                                                
* Return to Caller                                                              
     LM    R0,RF,FLOWSAV2          Restore the registers                        
     BR    RE                      Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                         Parse a Term                        *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcParse DC 0H                                                             
    LR    R6,R3                    Save current address                         
    XR    R5,R5                    Indicate no quotes (yet)                     
    DO FROM=(R4)                   Do for all chars                             
      DOEXIT (CLI,0(R3),EQ,C' '),    Exit loop if blank found          +        
               AND,(LTR,R5,R5,Z)       and not in quoted string.                
      IF (CLI,0(R3),EQ,C'''')        If quote located                           
        IF (LTR,R5,R5,NZ)              If already in quotes                     
          XR    R5,R5                    Indicates no quotes                    
        ELSE ,                         Else                                     
          LR    RF,R3                    Load quote address                     
          BCTR  RF,0                     Point to previous character            
*                                        FLOWSAV2+4 = saved A(stmt)             
          DO WHILE=(C,RF,GE,FLOWSAV2+4) Do while still in buffer                
            TRT 0(1,RF),FLOWPTRM_SPEC      Test for special char                
            DOEXIT (Z)                     End loop if special char             
            IF (CLI,0(RF),EQ,C'=')         If possible literal                  
              AHI   RF,1                     Point past '='                     
              DO WHILE=(CR,RF,LT,R3)         Do for numerics                    
                DOEXIT (CLI,0(RF),LT,C'0')     Exit if not numeric              
                AHI   RF,1                     Advance pointer                  
              ENDDO ,                        EndDo for numerics                 
              IF (CLI,0(RF),EQ,C'C'),OR,     If char string <OR>       +        
               (CLI,0(RF),EQ,C'G'),OR,       .. DBCS string <OR>       +        
               (CLI,0(RF),EQ,C'X')           .. hex string                      
                LR    R5,R3                    Save quote address               
                ASMLEAVE ,                     Exit the loop                    
              ENDIF ,                        EndIf char/DBCS/hex                
              LA    RF,1(,R3)                Point past quote                   
              LR    R0,R4                    Get length remaining               
              SHI   R0,1                     (same)                             
              IF (P)                         If more data                       
                DO FROM=(R0)                   Do for chars                     
                  DOEXIT (CLI,0(RF),EQ,C'''')    Exit if quote                  
                  AHI   RF,1                     Advance pointer                
                ENDDO ,                        EndDo for chars                  
                IF (LTR,R0,R0,NZ)              If matching quote                
                  LR    R5,R3                    Save quote address             
                  ASMLEAVE ,                     Exit the loop                  
                ENDIF ,                        EndIf matching quote             
              ENDIF ,                        EndIf more data                    
              ASMLEAVE ,                     Exit the loop                      
            ENDIF ,                        EndIf possible literal               
            BCTR  RF,0                     Decrement pointer                    
          ENDDO ,                        EndDo while still in buffer            
          LR    RF,R3                    Point to char before quote             
          BCTR  RF,0                     (same)                                 
          IF (TRT,0(1,RF),FLOWPTRM_ATTR,NZ) If not attribute                    
            LR    R5,R3                    Save address of quote                
          ENDIF ,                        EndIf                                  
        ENDIF ,                        EndIf already in quotes                  
      ENDIF ,                        EndIf quote located                        
      LA    R3,1(,R3)                Advance pointer                            
    ENDDO ,                        EndDo for all chars                          
    LR    R1,R3                    Compute length of term                       
    SR    R1,R6                    (same)                                       
    BR    RE                       Return                                       
                                                                                
FLOWDATA LOCTR ,                   Define data LOCTR                            
FLOWPTRM_SPEC DC  63X'00',193X'FF'   Special character table                    
              ORG FLOWPTRM_SPEC+C'(' Left paren                                 
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_SPEC+C')' Right paren                                
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_SPEC+C',' Comma                                      
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_SPEC+C'/' Slash                                      
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_SPEC+C'*' Asterisk                                   
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_SPEC+C'+' Plus                                       
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_SPEC+C'-' Minus                                      
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_SPEC+C' ' Blank                                      
              DC  X'00'              (same)                                     
              ORG ,                                                             
                                                                                
FLOWPTRM_ATTR DC  256X'FF'           Attribute reference table                  
              ORG FLOWPTRM_ATTR+C'd' Defined                                    
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'D' (same)                                     
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'i' Integer                                    
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'I' (same)                                     
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'k' Count                                      
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'K' (same)                                     
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'l' Length                                     
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'L' (same)                                     
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'n' Number                                     
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'N' (same)                                     
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'o' Operation code                             
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'O' (same)                                     
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C's' Scaling                                    
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'S' (same)                                     
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C't' Type                                       
              DC  X'00'              (same)                                     
              ORG FLOWPTRM_ATTR+C'T' (same)                                     
              DC  X'00'              (same)                                     
              ORG ,                                                             
FLOWCODE LOCTR ,                   Define code LOCTR                            
                                                                                
***************************************************************                 
*                                                             *                 
*                      Parse White Space                      *                 
*                                                             *                 
***************************************************************                 
FLOW_SrcAdvWhite DC 0H                                                          
    DO FROM=(R4)                   Do for remaining chars                       
      DOEXIT (CLI,0(R3),GT,C' ')     Exit if non-blank found                    
      AHI   R3,1                     Advance pointer                            
    ENDDO ,                        EndDo                                        
    BR    RE                       Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                 Get Listing Statement Offset                *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOW_LstGetStmtOff DC 0H                                                        
    LH    RF,FLOWSTMO              Get statement offset                         
    LTR   RF,RF                    Statement offset known ?                     
    BNZR  RE                       Return if yes                                
    CLC   AXPOPTS,=A(AXPOPDET)     Options summary line ?                       
    BNER  RE                       Return if not                                
    STM   R0,RF,FLOWSAV1           Save the registers                           
                                                                                
    LR    RE,R1                    Point past the line                          
    ALR   RE,R0                    (same)                                       
    LHI   R0,C'L'                  Search char = 'L'                            
    DO UNTIL=14 (8+4+2)            Do until finished                            
      SRST  RE,R1                    Search for 'L'                             
    ENDDO ,                        EndDo                                        
    IF (4)                         If 'L' found                                 
      SELECT CLC,0(9,RE),EQ          Select option                              
      WHEN (=CL9'LIST(121)')         When LIST(121)                             
        LHI   RF,41                    Set offset to 41                         
        STH   RF,FLOWSTMO              Save for later use                       
        MVC   FLOWMCNO,=H'116'         Offset to macroname (if present)         
        MVC   FLOWOPCO,=H'8'           Offset to opcode (in obj. code)          
      WHEN (=CL9'LIST(133)')         When LIST(133)                             
        LHI   RF,50                    Set offset to 50                         
        STH   RF,FLOWSTMO              Save for later use                       
        MVC   FLOWMCNO,=H'125'         Offset to macroname (if present)         
        MVC   FLOWOPCO,=H'10'          Offset to opcode (in obj. code)          
      WHEN (=CL9'LIST     ')         When LIST (VSE option)                     
        LHI   RF,41                    Set offset to 41                         
        STH   RF,FLOWSTMO              Save for later use                       
        MVC   FLOWMCNO,=H'116'         Offset to macroname (if present)         
        MVC   FLOWOPCO,=H'8'           Offset to opcode (in obj. code)          
      ENDSEL ,                       EndSel option                              
    ENDIF ,                        EndIf 'L' found                              
                                                                                
    LM    R0,RE,FLOWSAV1           Restore registers 0-14                       
    BR    RE                       Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                Get Listing OpCode Start Column              *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOW_LstGetOpCol DC 0H                                                          
    XR    RF,RF                    Set opcode column to zero                    
    CLC   AXPOPTS,=A(AXPSOMIN)     Source/object data ?                         
    BLR   RE                       Return if not                                
    CLC   AXPOPTS,=A(AXPSOTHR)     Source/object data ?                         
    BHR   RE                       Return if not                                
    CLC   AXPOPTS,=A(AXPSOCOM)     Comment ?                                    
    BER   RE                       Return if yes                                
*======================== REMOVED BY DEIS ============================*         
*   CLC   AXPOPTS,=A(AXPSOSIE)     Statement in error ?                         
*   BER   RE                       Return if yes                                
*=====================================================================*         
    STM   R0,RF,FLOWSAV1           Save the registers                           
                                                                                
***********************************************************************         
* Check for severity 4 warnings that we consider to be deadly, and              
* raise their severity to 8.                                                    
***********************************************************************         
                                                                                
    IF (CLC,AXPOPTS,EQ,=A(AXPSOERR)) Error message ?                            
*                                     Yes: examine it before returning          
*                                                                               
      LA    R2,WARNTAB              A(warning message table)                    
      USING WARNTABD,R2                                                         
      DO UNTIL=(CLI,0(R2),EQ,X'FF') loop until EOT                              
        L   R5,AXPBUFP              Get buffer address                          
        IF  (CLC,MSGCODE,EQ,4(R5))  if we have a match on the message:          
          L     R3,AERRMSG            A(additional message)                     
          L     R4,ERRMSGL            L'additional message                      
          ST    R4,AXPERRL            set up error message length               
          L     RE,AXPERRP            Load error buffer address                 
          BCTR  R4,0                  for EX instruction                        
          EXRL  R4,*+10                                                         
          J     *+10                                                            
          MVC   0(0,RE),0(R3)         Set error message text                    
          MVC   AXPSEVC,=F'8'         Push error message severity to 8          
          DOEXIT ,                                                              
        ENDIF ,                                                                 
        LA    R2,WARNTBLQ(R2)      no match: bump to next table entry           
      ENDDO                                                                     
      DROP  R2                                                                  
      XR    RF,RF                  Set opcode column to zero                    
      LM    R0,RE,FLOWSAV1         Restore registers 0-14                       
      BR    RE                     now return                                   
    ENDIF ,                                                                     
                                                                                
    DO ,                           Do for opcode column                         
      LR    RE,R1                    Point to last byte of stmt num             
      SHI   RE,2                     (same)                                     
      DOEXIT (CLI,0(RE),LT,C'0')     Exit if not valid stmt number              
      DOEXIT (CLI,0(RE),GT,C'9')     Exit if not valid stmt number              
      IF (CLI,0(R1),GT,C' ')         If label present                           
        DO FROM=(R0)                   Do for label                             
          DOEXIT (CLI,0(R1),LE,C' ')     Exit if blank found                    
          LA    R1,1(,R1)                Advance pointer                        
        ENDDO ,                        EndDo                                    
      ENDIF ,                        EndIf label present                        
      DOEXIT (LTR,R0,R0,NP)          Exit if nothing left                       
      DO FROM=(R0)                   Do until opcode found                      
        DOEXIT (CLI,0(R1),GT,C' ')     Exit if opcode found                     
        LA    R1,1(,R1)                Advance pointer                          
      ENDDO ,                        EndDo                                      
      DOEXIT (LTR,R0,R0,NP)          Exit if nothing left                       
      L     RF,FLOWSAV1              Compute opcode column                      
      SR    RF,R0                    (same)                                     
      AHI   RF,1                     (same)                                     
    ENDDO ,                        EndDo for opcode column                      
                                                                                
* Return to Caller                                                              
    LM    R0,RE,FLOWSAV1           Restore registers 0-14                       
    BR    RE                       Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*           Get Listing First Non-Blank Start Column          *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOW_LstGetNonBlank DC 0H                                                       
    XR    RF,RF                    Set non-blank column to zero                 
    CLC   AXPOPTS,=A(AXPSOMIN)     Source/object data ?                         
    BLR   RE                       Return if not                                
    CLC   AXPOPTS,=A(AXPSOTHR)     Source/object data ?                         
    BHR   RE                       Return if not                                
    CLC   AXPOPTS,=A(AXPSOCOM)     Comment ?                                    
    BER   RE                       Return if yes                                
*======================== REMOVED BY DEIS ============================*         
*   CLC   AXPOPTS,=A(AXPSOSIE)     Statement in error ?                         
*   BER   RE                       Return if yes                                
*=====================================================================*         
    CLC   AXPOPTS,=A(AXPSOERR)     Error message ?                              
    BER   RE                       Return if yes                                
    STM   R0,RF,FLOWSAV1           Save the registers                           
                                                                                
    DO FROM=(R0)                   Do for all chars                             
      IF (CLI,0(R1),GT,C' ')         If non-blank character                     
        L     RF,FLOWSAV1              Compute non-blank column                 
        SR    RF,R0                    (same)                                   
        AHI   RF,1                     (same)                                   
        ASMLEAVE ,                     Exit the loop                            
      ENDIF ,                        EndIf                                      
      LA    R1,1(,R1)                Advance pointer                            
    ENDDO ,                        EndDo for all chars                          
                                                                                
* Return to Caller                                                              
    LM    R0,RE,FLOWSAV1           Restore registers 0-14                       
    BR    RE                       Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*           Add/Remove Listing Flow Bars Controls             *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOW_LstAddRemBars DC 0H                                                        
    STM   R0,RF,FLOWSAV1           Save the registers                           
    USING FLOWBar,R3               Synchronize FLOWBar DSECT                    
                                                                                
    LR    R4,R1                                                                 
                                                                                
    ALR   R1,RF                    Point to the opcode                          
    SHI   R1,1                     (same)                                       
    MVC   FLOWMACW(8),0(R1)        Copy to work area                            
    OC    FLOWMACW(8),=CL8' '      Make upper case English                      
                                                                                
* Terminate Current Flow Bar                                                    
    DO ,                           Do for flow bar terminate                    
      ICM   R3,B'1111',FLOWBARP      Get current flow bar pointer               
      DOEXIT (Z)                     Exit if no current entry                   
      XR    RE,RE                    Insert entry type                          
      IC    RE,FLOWBarType           (same)                                     
      CASENTRY RE                    Cases for entry type                       
      CASE 1                         FlowBarTyIf                                
        IF (CLC,=C'ENDIF ',EQ,FLOWMACW) If ENDIF                                
          XR    RE,RE                    Show terminator found                  
        ENDIF ,                        EndIf                                    
      CASE 2                         FlowBarTyDo                                
        IF (CLC,=C'ENDDO ',EQ,FLOWMACW) If ENDDO                                
          XR    RE,RE                    Show terminator found                  
        ENDIF ,                        EndIf                                    
      CASE 3                         FlowBarTySel                               
        IF (CLC,=C'ENDSEL ',EQ,FLOWMACW) If ENDSEL                              
          XR    RE,RE                    Show terminator found                  
        ENDIF ,                        EndIf                                    
      CASE 4                         FlowBarTyCase                              
        IF (CLC,=C'ENDCASE ',EQ,       If ENDCASE                      +        
               FLOWMACW)               ..                                       
          XR    RE,RE                    Show terminator found                  
        ENDIF ,                        EndIf                                    
      ENDCASE ,                      EndCases for entry type                    
      DOEXIT (LTR,RE,RE,NZ)          Exit if no terminator                      
      SHI   R3,FLOWBarLen            Point to previous entry                    
      LA    R0,FLOWBARS              Point to top of table                      
      IF (CR,R3,LT,R0)               If prior to table start                    
        XR    R3,R3                    Zero the pointer                         
      ENDIF ,                        EndIf                                      
      ST    R3,FLOWBARP              Save new flow bar pointer                  
    ENDDO ,                        EndDo for flow bar terminate                 
                                                                                
* Add New Flow Bar                                                              
    DO ,                           Do for new flow bar                          
      XR    R0,R0                    Show no new structure                      
      SELECT ,                       Select for opcode                          
      WHEN (CLC,=C'IF ',EQ,FLOWMACW) IF                                         
        LHI   R0,FLOWBarTyIf           Show IF structure                        
      WHEN (CLC,=C'DO ',EQ,FLOWMACW) DO                                         
        LHI   R0,FLOWBarTyDo           Show DO structure                        
      WHEN (CLC,=C'SELECT ',         SELECT                            +        
               EQ,FLOWMACW)          ..                                         
        LHI   R0,FLOWBarTySel          Show SELECT structure                    
      WHEN (CLC,=C'CASENTRY ',       CASENTRY                          +        
               EQ,FLOWMACW)          ..                                         
        LHI   R0,FLOWBarTyCase         Show CASE structure                      
      ENDSEL ,                       EndSel for opcode                          
      DOEXIT (LTR,R0,R0,Z)           Exit if no new structure                   
      LA    R2,FLOWCHAR              Point to 1st flow bar char                 
      IF (ICM,R3,B'1111',FLOWBARP,Z) If flow bar pointer not set                
        LA    R3,FLOWBARS              Point to flow bar table                  
      ELSE ,                         Else                                       
*       IF CH,RF,GT,FLOWBarCol         If new structure indented                
          SELECT CLC,FLOWBarChar,EQ      Select current bar char                
          WHEN (FLOWCHAR)                1st flow bar char                      
            LA    R2,FLOWCHAR+1            Set to 2nd flow bar char             
          WHEN (FLOWCHAR+1)              2nd flow bar char                      
            LA    R2,FLOWCHAR+2            Set to 3rd flow bar char             
          WHEN (FLOWCHAR+2)              3rd flow bar char                      
            LA    R2,FLOWCHAR              Set to 1st flow bar char             
          ENDSEL ,                       EndSel current bar char                
*       ENDIF ,                        EndIf new structure indented             
        LA    R3,FLOWBarLen(,R3)       Advance pointer                          
        LA    RE,FLOWBARP              Point past table                         
        DOEXIT (CR,R3,GE,RE)           Exit if too many entries                 
      ENDIF ,                        EndIf pointer not set                      
      ST    R3,FLOWBARP              Save new flow bar pointer                  
      STH   RF,FLOWBarCol            Set column number                          
      STC   R0,FLOWBarType           Set structure type                         
      MVC   FLOWBarChar,0(R2)        Set flow bar character                     
    ENDDO ,                        EndDo for new flow bar                       
                                                                                
    DROP  R3                       Drop FLOWBar DSECT                           
                                                                                
* Return to Caller                                                              
    LM    R0,RF,FLOWSAV1           Restore the registers                        
    BR    RE                       Return                                       
                                                                                
*******************************************************************             
*                                                                 *             
*                                                                 *             
* Change SPM-generated BRC instructions to use extended mnemonics *             
*                                                                 *             
*                                                                 *             
*******************************************************************             
FLOW_LstUseBRCMnemonics DC 0H                                                   
                                                                                
* The IBM Structured Programming Macros generate BRC instructions               
* with explicit branch masks, instead of the more readable extended             
* branch mnemonics. This routine identifies SPM-generated jump                  
* instructions within the listing (only when PRINT GEN is on,                   
* obviously), and then modifies the listing line to print the most              
* appropriate extended branch mnemonic in place of the BRC and mask.            
*                                                                               
* The basic strategy is to maintain a global "post-compare" flag which          
* we set when we see a *compare* instruction. When we encounter an SPM-         
* generated jump, we use the status of that flag to determine which             
* extended mnemonic to build. E.g., if we see a "BRC 8," after a                
* compare, then we replace the "BRC 8," with "BE". Otherwise, we                
* replace it with "BZ".                                                         
                                                                                
    STM   R0,RF,FLOWSAV1           Save the registers                           
                                                                                
    LR    R7,RF                    Save displacement to mnemonic                
    LR    R4,R1                    A(source statement)                          
                                                                                
    ALR   R1,RF                    Point to the mnemonic                        
    SHI   R1,1                     (same)                                       
    MVC   FLOWMACW(8),0(R1)        Copy to work area                            
    OC    FLOWMACW(8),=CL8' '      Make upper case English                      
                                                                                
    L     R5,AXPBUFP               A(listing line buffer)                       
    AH    R5,FLOWSTMO              R5 = A(starting statement offset)            
                                                                                
    DO LABEL=CHEK4SPM              DO for modify BRC instruction                
                                                                                
*     Identify SPM macro calls consisting *only* of an explicitly-              
*     named branch condition (e.g., "IF (EQ)" ). These are treated as           
*     compares (i.e., if we see one, we set the "post-compare" flag).           
      IF (CLC,=C'IF ',EQ,FLOWMACW),OR, see if it's an SPM              +        
               (CLC,=C'ELSEIF ',EQ,FLOWMACW),OR,                       +        
               (CLC,=C'DO ',EQ,FLOWMACW),OR,                           +        
               (CLC,=C'DOEXIT ',EQ,FLOWMACW),OR,                       +        
               (CLC,=C'WHEN ',EQ,FLOWMACW)                                      
        LA   RF,1(,R5)                   start scan in col 2 of stmt            
        LHI  R2,FLOWVSTL-1               stop scanning after last col.          
        DO FROM=(R2)                     check all columns                      
          IF (CLC,=C' (H) ',EQ,0(RF)),OR,                              +        
               (CLC,=C' (GT) ',EQ,0(RF)),OR,                           +        
               (CLC,=C' (L) ',EQ,0(RF)),OR,                            +        
               (CLC,=C' (LT) ',EQ,0(RF)),OR,                           +        
               (CLC,=C' (E) ',EQ,0(RF)),OR,                            +        
               (CLC,=C' (EQ) ',EQ,0(RF)),OR,                           +        
               (CLC,=C' (NH) ',EQ,0(RF)),OR,                           +        
               (CLC,=C' (LE) ',EQ,0(RF)),OR,                           +        
               (CLC,=C' (NL) ',EQ,0(RF)),OR,                           +        
               (CLC,=C' (GE) ',EQ,0(RF)),OR,                           +        
               (CLC,=C' (NE) ',EQ,0(RF))                                        
            OI  FLOWPFL1,FLOWPCMP            set "post-compare" flag            
            DOEXIT DO=CHEK4SPM               *EXIT*                             
          ENDIF                                                                 
          LA    RF,1(,RF)                  bump to next character               
        ENDDO ,                          loop back                              
      ENDIF ,                                                                   
                                                                                
*     If we reach this point, we know we're not looking at an explicit          
*     condition such as "IF (EQ)" . So now we check to see if it's a            
*     compare instruction. If it is, we set the "post-compare" flag.            
*     For the purpose of identifying compare instructions, we really            
*     only want to consider statements generated by an SPM.                     
*     Unfortunately, the assembler listing doesn't consistently tell us         
*     *which* macro generated which instructions. However, we do know           
*     if a statement was generated by *some* macro, because the                 
*     assembler indicates this by placing a hyphen in a fixed position          
*     within the sequence number area. So the best we can do is to use          
*     that hyphen as an indicator, and if the statement wasn't                  
*     generated by a macro, we don't look at it any further.                    
      L     R1,AXPBUFP               A(listing line buffer)                     
      AH    R1,FLOWMCNO              A(macroname) if provided by assem.         
      DOEXIT (CLIY,-1(R1),NE,C'-')   exit if not part of a macro expan.         
                                                                                
      LA    R3,CMPRINST                A(opcode table)                          
      USING CMPRINSD,R3                                                         
      DO UNTIL=(CLI,0(R3),EQ,X'FF')    loop until EOT                           
        LLC   R1,CMPRMNEL                L'mnemonic                             
        EXRL  R1,*+10                                                           
        J     *+10                                                              
        CLC   FLOWMACW(0),CMPRMNEM       match on mnemonic?                     
        IF (EQ)                                                                 
          OI    FLOWPFL1,FLOWPCMP          yes: set "post-compare" flag         
          DOEXIT DO=CHEK4SPM               *EXIT*                               
        ENDIF                                                                   
        LA    R3,CMPRINLQ(,R3)           bump to next opcode in table           
      ENDDO ,                          loop back                                
      DROP R3                                                                   
                                                                                
*     We're not looking at a compare, so now we see if it's a jump.             
      LA    R3,JUMPINST              A(jump opcode table)                       
      USING JUMPINSD,R3                                                         
      DO INF                         loop until opcode found or EOT             
        L     R1,AXPBUFP               A(listing line buffer)                   
        AH    R1,FLOWOPCO              A(opcode from object code)               
        CLC   JUMPOPCD,0(R1)           match on opcode?                         
        DOEXIT (EQ)                    yes: it's a jump                         
        LA    R3,JUMPTBLQ(,R3)         bump to next opcode in table             
        DOEXIT (CLI,0(R3),EQ,X'FF'),DO=CHEK4SPM  EOT: *exit*                    
      ENDDO ,                                                                   
                                                                                
*     It is a jump, so now see if it was generated by an SPM. If it             
*     was, then we overwrite the BRC with the correct extended                  
*     mnemonic.                                                                 
      LA    R5,1(,R5)                start scanning in col 2 of stmt            
      LHI   R2,FLOWVSTL-1            stop scanning after last column            
      DO  FROM=(R2)                  check all columns                          
        IF (CLC,=C'#@LB',EQ,0(R5))     if it's an SPM-generated label:          
          IF (CLIY,-1(R5),NE,C','),ORIF,   must be preceded by comma...+        
               (CLIY,-2(R5),NE,C'1'),AND,  and mask 1, or...           +        
               (CLIY,-2(R5),NE,C'2'),AND,  and mask 2, or...           +        
               (CLIY,-2(R5),NE,C'3'),AND,  and mask 13, or...          +        
               (CLIY,-2(R5),NE,C'4'),AND,  and mask 4, 14, or...       +        
               (CLIY,-2(R5),NE,C'5'),AND,  and mask 15, or...          +        
               (CLIY,-2(R5),NE,C'7'),AND,  and mask 7, or...           +        
               (CLIY,-2(R5),NE,C'8'),ORIF, and mask 8, or...           +        
               (CLIY,-3(R5),NE,C'1'),AND,  and mask 11, 13, 14, 15     +        
               (CLIY,-3(R5),NE,C' ')                                            
            L     RE,AXPERRP              Load error buffer address             
            MVC   0(L'FLOWERR4,RE),FLOWERR4 Set error message text              
            MVC   AXPERRL,=A(L'FLOWERR4)  Set error message length              
            MVC   AXPSEVC,=F'4'           Set error message severity            
            DOEXIT DO=CHEK4SPM            (should never happen!)                
          ENDIF                                                                 
                                                                                
          DO UNTIL=(CLIY,-1(R5),EQ,C' ')   blank out the mask and comma         
            MVIY  -1(R5),C' '                                                   
            BCTR  R5,0                       back up 1 byte                     
          ENDDO ,                                                               
          DO UNTIL=(CLC,=C'BRC ',EQ,0(R5)) point R5 to "BRC" mnemonic           
            BCTR  R5,0                                                          
          ENDDO ,                                                               
                                                                                
*         overwrite the "BRC" with the appropriate extended mnemonic            
          IF (TM,FLOWPFL1,FLOWPCMP,O)      BRC preceded by a compare?           
            MVC   0(L'JUMPJCMP,R5),JUMPJCMP  yes: use those ext. mnem.          
          ELSE ,                           no:                                  
            MVC   0(L'JUMPJOTH,R5),JUMPJOTH  use other ext. mnemonic            
          ENDIF ,                                                               
                                                                                
*         prettify BRC if generated by ASMMIFPR macro (for alignment)           
          L     R1,AXPBUFP                 A(listing line buffer)               
          AH    R1,FLOWMCNO                A(macroname) if present              
          BCTR  R1,0                       R1 = A(hyphen) if present            
          IF (CLC,=C'-ASMMI',EQ,0(R1)),AND,  prettify ASMMIFPR output  +        
               (CLC,=C'     ',EQ,5(R5))         (if it's safe to do so)         
            MVC   5(5,R5),0(R5)              shift mnemonic...                  
            MVC   0(5,R5),=C'     '          ...5 bytes to the right            
            AHI   R7,5                       adjust flowbar placement           
          ENDIF                                                                 
          NI  FLOWPFL1,X'FF'-FLOWPCMP      reset "post-compare" flag            
          DOEXIT ,                                                              
        ENDIF ,                                                                 
        LA    R5,1(R5)             bump to next char. in listing line           
      ENDDO  ,                   loop back                                      
      DROP R3                                                                   
                                                                                
    ENDDO ,                    EndDo CHEK4SPM                                   
                                                                                
* Return to Caller                                                              
    LR    RF,R7                (Possibly adjusted) mnemonic column              
    LM    R0,RE,FLOWSAV1       Restore the registers                            
    BR    RE                   Return                                           
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                Format Flow Bars on the Listing              *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOW_LstFormatBars DC 0H                                                        
    STM   R0,RF,FLOWSAV1           Save the registers                           
                                                                                
    LHI   RE,X'7FFF'               Get big column number                        
    LA    R0,FLOWBARS              Point to top of table                        
    IF (ICM,R3,B'1111',FLOWBARP,NZ) If flow bars are needed                     
      DO INF                         Do for flow bar formatting                 
        USING FLOWBar,R3               Synchronize FLOWBar DSECT                
        IF (CH,RF,GT,FLOWBarCol)       If indentation not overlapped            
*         DOEXIT CH,RE,LE,FLOWBarCol     Exit if overlapped indentation         
          LH    RE,FLOWBarCol            Get column number                      
          LA    R2,0(RE,R1)              Point to byte in listing               
          SHI   R2,1                     (same)                                 
          IF (CLI,0(R2),EQ,C' ')         If blank spot                          
            MVC   0(1,R2),FLOWBarChar      Copy flow bar character              
          ENDIF ,                        EndIf                                  
        ENDIF ,                        EndIf indentation not overlapped         
        SHI   R3,FLOWBarLen            Point to previous entry                  
        DOEXIT (CR,R3,LT,R0)           Exit if prior to table start             
        DROP  R3                       Drop FLOWBar DSECT                       
      ENDDO ,                        EndDo for flow bar terminate               
    ENDIF ,                        EndIf flow bars are needed                   
                                                                                
* Return to Caller                                                              
    LM    R0,RF,FLOWSAV1           Restore the registers                        
    BR    RE                       Return                                       
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                           Equates                           *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOWVSTL EQU   71                  Statement length                             
CONTINUATION_START_COLUMN EQU 16   Continued statement start column             
                                                                                
***************************************************************                 
*                                                             *                 
*                                                             *                 
*                          Constants                          *                 
*                                                             *                 
*                                                             *                 
***************************************************************                 
FLOWDATA  LOCTR ,                  Define data LOCTR                            
                                                                                
FLOWDCBM  DCB  DSORG=PS,           Sequential data set                 +        
               MACRF=GL,           Get locate mode                     +        
               DDNAME=SYSIN,       DD name = SYSIN                     +        
               DCBE=FLOWDCBE       DCBE address                                 
FLOWDCBML EQU  *-FLOWDCBM          Length of DCB model                          
FLOWDCBE  DCBE EODAD=FLOW_SrcEod,  End of data address                 +        
               RMODE31=BUFF        Use 31-bit buffers                           
FLOWDCBEL EQU  *-FLOWDCBE          Length of DCBE model                         
FLOWRDFM  RDJFCB FLOWDCBM,MF=L     RDJFCB model                                 
FLOWRDFML EQU  *-FLOWRDFM          Length of RDJFCB model                       
FLOW_SrcOpenM  OPEN FLOWDCBM,      OPEN model                          +        
               MODE=31,MF=L        (same)                                       
FLOW_SrcOpenML EQU *-FLOW_SrcOpenM Length of OPEN model                         
                                                                                
FLOWERR1 DC    C'First column of continued statement is non-blank'              
FLOWERR2 DC    C'Not enough room for continued operand data'                    
FLOWERR3 DC    C'Not enough room for all of continued comment'                  
FLOWERR4 DC    C'Please notify David E.: unexpected SPM expansion in li+        
               sting (this is not fatal).'                                      
                                                                                
FLOWCHAR DC    CL3'º:|'            Flow bar characters                          
                                                                                
* NOTE: the FLOWERRM table is only used by the LIBRARY exit, which              
*       we currently do not invoke.                                             
FLOWERRM DC   0CL8                 Macros with syntax errors                    
         DC    CL8'EVENTS'         (same)                                       
         DC    CL8'IAZSMF84'       (same)                                       
         DC    CL8'IEAMSTS'        (same)                                       
         DC    CL8'IEFSSSA'        (same)                                       
         DC    CL8'IEZVG111'       (same)                                       
         DC    CL8'IFGACBVS'       (same)                                       
         DC    CL8'IFGEXLVS'       (same)                                       
         DC    CL8'IHASPTRC'       (same)                                       
         DC    CL8'IHATMTRC'       (same)                                       
         DC    CL8'IRDDFSD'        (same)                                       
*********DC    CL8'TRKCALC'        (same)                                       
         DC    CL8'#DIE'           (same)                                       
FLOWERRM# EQU  (*-FLOWERRM)/8      Number of table entries                      
                                                                                
FLOWDDSM DC   0CL9                 DDS macros enabled for continuation          
         DC    CL8'GOTO1',C' '     (same)                                       
         DC    CL8'EDIT',C' '      (same)                                       
         DC    CL8'GOTOR',C' '     (same)                                       
         DC    CL8'GOTOX',C' '     (same)                                       
         DC    CL8'CURED',C' '     (same)                                       
         DC    CL8'EDITR',C' '     (same)                                       
         DC    CL8'KEYDEF',C' '    (same)                                       
*                                  IBM Structured Programming Macros            
         DC    CL8'IF',C' '        (same)                                       
         DC    CL8'ELSEIF',C' '    (same)                                       
         DC    CL8'DO',C' '        (same)                                       
         DC    CL8'DOEXIT',C' '    (same)                                       
         DC    CL8'SELECT',C' '    (same)                                       
         DC    CL8'WHEN',C' '      (same)                                       
*                                                                               
         DC    X'FF'               EOT                                          
                                                                                
         EJECT                                                                  
                                                                                
   LTORG ,                         Define literals                              
                                                                                
         EJECT                                                                  
WARNTAB  DS    0D                                                               
*                                                                               
* NOTE: WHEN NEW WARNING MESSAGES ARE ADDED TO THIS TABLE, WE SHOULD            
*       ALSO ADD CORRESPONDING CODE TO OUR POST-ASSEMBLY INTEGRITY              
*       CHECK.                                                                  
*       AS OF OCT/2017, THE MEMBERS THAT MIGHT NEED CHANGING ARE:               
*       'PANAPT.DDS.PARMLIB(ACHKCOP1)'                                          
*       'PANAPT.DDS.PARMLIB(ACHKCOP5)'                                          
*                                                                               
 DC C'ASMA309W',A(ERRMSG1),A(ERRMSG1L) PAGE 0 REFERENCE                         
 DC C'ASMA300W',A(ERRMSG2),A(ERRMSG2L) THIS USING OVERRIDDEN                    
 DC C'ASMA301W',A(ERRMSG2),A(ERRMSG2L) PRIOR USING OVERRIDDEN                   
 DC C'ASMA320W',A(ERRMSG3),A(ERRMSG3L) IMMEDIATE OPER. MAGNITUDE                
 DC C'ASMA424W',A(ERRMSG4),A(ERRMSG4L) *PROCESS STMT CONTINUATION ERROR         
 DC C'ASMA430W',A(ERRMSG5),A(ERRMSG5L) BLANK CONT. COL W/ CONT'D STMT           
 DC C'ASMA431W',A(ERRMSG5),A(ERRMSG5L) BLANK CONT. COL, NO CONT'D STMT          
 DC C'ASMA432W',A(ERRMSG5),A(ERRMSG5L) COMMA OMITTED FROM CONT'D STMT           
 DC C'ASMA433W',A(ERRMSG5),A(ERRMSG5L) CONTINUED STMT NOT IN COL. 16            
 DC C'ASMA036W',A(ERRMSG6),A(ERRMSG6L) Reentrant check failed                   
 DC C'ASMA042W',A(ERRMSG7),A(ERRMSG7L) Length attribute unavailable             
 DC X'FF'                                                                       
         SPACE 3                                                                
ERRMSG1  DC    C'If this is a legitimate page 0 reference, specify an e+        
               xplicit base register of 0, e.g.: "L  Rx,16(,0)"'                
ERRMSG1L EQU   *-ERRMSG1                                                        
*                                                                               
ERRMSG2  DC    C'Ambiguous USINGs. (The assembler has chosen the USING +        
               statement with the higher-numbered register.)'                   
ERRMSG2L EQU   *-ERRMSG2                                                        
*                                                                               
ERRMSG3  DC    C'If operand is legitimate, suppress this warning via "A+        
               CONTROL TYPECHECK(NOMAGNITUDE)"'                                 
ERRMSG3L EQU   *-ERRMSG3                                                        
*                                                                               
ERRMSG4  DC    C'Fatal error."'                                                 
ERRMSG4L EQU   *-ERRMSG4                                                        
*                                                                               
ERRMSG5  DC    C'Fix this error. If the statement is TRULY correct, sup+        
               press the warning via "ACONTROL FLAG(NOCONT)"'                   
ERRMSG5L EQU   *-ERRMSG5                                                        
*                                                                               
ERRMSG6  DC    C'Fatal error. Fix the reentrancy issue, or change the R+        
               SECT to a CSECT.'                                                
ERRMSG6L EQU   *-ERRMSG6                                                        
*                                                                               
ERRMSG7  DC    C'Either provide an explicit length attribute, or modify+        
                the code so the assembler can derive it.'                       
ERRMSG7L EQU   *-ERRMSG7                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
CMPRINST DS    0D                                                               
*                                                                               
* This is a table of compare instruction mnemonics. It's preferable             
* (but not mandatory) to update this table as new compare instructions          
* are introduced into the architecture.                                         
*                                                                               
* The table is used when we change SPM-generated "BRC" instructions             
* into their equivalent extended-mnemonic instructions. For example,            
* a "BRC 8," will be printed as "BE" if it follows a compare                    
* instruction, but "BZ" if it follows a TM or an arithmetic                     
* instruction. If this table is not kept up-to-date, then we might              
* generate a confusing extended branch mnemonic in the listing (but             
* this is not fatal).                                                           
*                                                                               
         DC    AL1(1),CL6'C'                                                    
         DC    AL1(3),CL6'CFI'                                                  
         DC    AL1(2),CL6'CG'                                                   
         DC    AL1(3),CL6'CGF'                                                  
         DC    AL1(4),CL6'CGFI'                                                 
         DC    AL1(4),CL6'CGFR'                                                 
         DC    AL1(5),CL6'CGFRL'                                                
         DC    AL1(3),CL6'CGH'                                                  
         DC    AL1(4),CL6'CGHI'                                                 
         DC    AL1(5),CL6'CGHRL'                                                
         DC    AL1(5),CL6'CGHSI'                                                
         DC    AL1(4),CL6'CGIB'                                                 
         DC    AL1(4),CL6'CGIJ'                                                 
         DC    AL1(4),CL6'CGIT'                                                 
         DC    AL1(3),CL6'CGR'                                                  
         DC    AL1(4),CL6'CGRB'                                                 
         DC    AL1(4),CL6'CGRJ'                                                 
         DC    AL1(4),CL6'CGRL'                                                 
         DC    AL1(4),CL6'CGRT'                                                 
         DC    AL1(2),CL6'CH'                                                   
         DC    AL1(3),CL6'CHF'                                                  
         DC    AL1(4),CL6'CHHR'                                                 
         DC    AL1(5),CL6'CHHSI'                                                
         DC    AL1(3),CL6'CHI'                                                  
         DC    AL1(4),CL6'CHLR'                                                 
         DC    AL1(4),CL6'CHRL'                                                 
         DC    AL1(4),CL6'CHSI'                                                 
         DC    AL1(3),CL6'CHY'                                                  
         DC    AL1(3),CL6'CIB'                                                  
         DC    AL1(3),CL6'CIH'                                                  
         DC    AL1(3),CL6'CIJ'                                                  
         DC    AL1(3),CL6'CIT'                                                  
         DC    AL1(2),CL6'CL'                                                   
         DC    AL1(3),CL6'CLC'                                                  
         DC    AL1(4),CL6'CLCL'                                                 
         DC    AL1(5),CL6'CLCLE'                                                
         DC    AL1(5),CL6'CLCLU'                                                
         DC    AL1(6),CL6'CLFHSI'                                               
         DC    AL1(4),CL6'CLFI'                                                 
         DC    AL1(5),CL6'CLFIT'                                                
         DC    AL1(3),CL6'CLG'                                                  
         DC    AL1(4),CL6'CLGF'                                                 
         DC    AL1(5),CL6'CLGFI'                                                
         DC    AL1(5),CL6'CLGFR'                                                
         DC    AL1(6),CL6'CLGFRL'                                               
         DC    AL1(6),CL6'CLGHRL'                                               
         DC    AL1(6),CL6'CLGHSI'                                               
         DC    AL1(5),CL6'CLGIB'                                                
         DC    AL1(5),CL6'CLGIJ'                                                
         DC    AL1(5),CL6'CLGIT'                                                
         DC    AL1(4),CL6'CLGR'                                                 
         DC    AL1(5),CL6'CLGRB'                                                
         DC    AL1(5),CL6'CLGRJ'                                                
         DC    AL1(5),CL6'CLGRL'                                                
         DC    AL1(5),CL6'CLGRT'                                                
         DC    AL1(4),CL6'CLGT'                                                 
         DC    AL1(4),CL6'CLHF'                                                 
         DC    AL1(5),CL6'CLHHR'                                                
         DC    AL1(6),CL6'CLHHSI'                                               
         DC    AL1(5),CL6'CLHLR'                                                
         DC    AL1(5),CL6'CLHRL'                                                
         DC    AL1(3),CL6'CLI'                                                  
         DC    AL1(4),CL6'CLIB'                                                 
         DC    AL1(4),CL6'CLIH'                                                 
         DC    AL1(4),CL6'CLIJ'                                                 
         DC    AL1(4),CL6'CLIY'                                                 
         DC    AL1(3),CL6'CLM'                                                  
         DC    AL1(4),CL6'CLMH'                                                 
         DC    AL1(4),CL6'CLMY'                                                 
         DC    AL1(3),CL6'CLR'                                                  
         DC    AL1(4),CL6'CLRB'                                                 
         DC    AL1(4),CL6'CLRJ'                                                 
         DC    AL1(4),CL6'CLRL'                                                 
         DC    AL1(4),CL6'CLRT'                                                 
         DC    AL1(4),CL6'CLST'                                                 
         DC    AL1(3),CL6'CLT'                                                  
         DC    AL1(3),CL6'CLY'                                                  
         DC    AL1(2),CL6'CP'                                                   
         DC    AL1(2),CL6'CR'                                                   
         DC    AL1(3),CL6'CRB'                                                  
         DC    AL1(3),CL6'CRJ'                                                  
         DC    AL1(3),CL6'CRL'                                                  
         DC    AL1(3),CL6'CRT'                                                  
         DC    AL1(2),CL6'CS'                                                   
         DC    AL1(3),CL6'CSG'                                                  
         DC    AL1(3),CL6'CSP'                                                  
         DC    AL1(4),CL6'CSPG'                                                 
         DC    AL1(4),CL6'CSST'                                                 
         DC    AL1(3),CL6'CSY'                                                  
         DC    AL1(4),CL6'CUSE'                                                 
         DC    AL1(2),CL6'CY'                                                   
         DC    X'FF'               table terminator                             
         SPACE 3                                                                
JUMPINST DS    0D                                                               
         DC    C'A704',CL5'JNOP',CL5'JNOP'                                      
         DC    C'A714',CL5'JO',CL5'JO'                                          
         DC    C'A724',CL5'JH',CL5'JP'                                          
         DC    C'A744',CL5'JL',CL5'JM'                                          
         DC    C'A774',CL5'JNE',CL5'JNZ'                                        
         DC    C'A784',CL5'JE',CL5'JZ'                                          
         DC    C'A7B4',CL5'JNL',CL5'JNM'                                        
         DC    C'A7D4',CL5'JNH',CL5'JNP'                                        
         DC    C'A7E4',CL5'JNO',CL5'JNO'                                        
         DC    C'A7F4',CL5'J',CL5'J'                                            
         DC    X'FF'               table terminator                             
         EJECT                                                                  
   DC    0D                        Align to doubleword for code                 
                                                                                
   END ,                                                                        
