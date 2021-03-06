*          DATA SET FRFTSBTBWI AT LEVEL 007 AS OF 05/01/02                      
*PHASE FTPSBTWI,*,NOAUTO                                                        
*************************** use special panS procs                              
DVGCXASM START                                                                  
* /***START OF SPECIFICATIONS*****************************************/         
* /*                                                                 */         
* /*      MODULE-NAME: DVGCXASM                                      */         
* /*                                                                 */         
* /*      CSECT  NAME: DVGCXASM                                      */         
* /*                                                                 */         
* /*                                                                 */         
* /*      DESCRIPTIVE NAME:                                          */         
* /*                 SAMPLE PROGRAM IN ASSEMBLER FOR THE NETVIEW FTP */         
* /*                 APPLICATION PROGRAM INTERFACE                   */         
* /*                                                                 */         
* /*      COPYRIGHT: 5685-108 (C) COPYRIGHT IBM CORP. 1991           */         
* /*                 LICENSED MATERIALS - PROPERTY OF IBM            */         
* /*                 SEE COPYRIGHT INSTRUCTIONS, G120-2083           */         
* /*                                                                 */         
* /*      OPERATION: THE PROGRAM                                     */         
* /*                 - INITIALIZES THE APL                           */         
* /*                 - ADDS A REQUEST TO BE TRANSFERRED              */         
* /*                 - QUERIES THE STATUS OF THE REQUEST             */         
* /*                                                                 */         
* /*      OUTPUT:                                                    */         
* /*             THE PROGRAM REPORTS THE RETURN AND REASON CODES     */         
* /*             FOR THE ADD COMMAND ON OUTDD.                       */         
* /*                                                                 */         
* /*             THE PROGRAM REPORTS THE QSR FOR THE QUERY           */         
* /*             COMMAND ON OUTDD.                                   */         
* /*                                                                 */         
* /*             ERROR MESSAGES ARE ISSUED IN CASE OF AN INTERFACE   */         
* /*             ERROR.                                              */         
* /*                                                                 */         
* /*             NOTE: Include DD statment in JCL:                   */         
* /*                   OUTDD DD SYSOUT=*                             */         
* /*                                                                 */         
* /*      CONTROL BLOCKS (MAPPING MACROS):                           */         
* /*             DVGAPL   - APPLICATION PROGRAM PARAMETER LIST       */         
* /*             DVGAPX   - APPLICATION PROGRAM PARAMETER LIST EX-   */         
* /*                        TENSION.  NEEDED FOR PO MEMBERS HANDLING */         
* /*             DVGQSR   - QUERY-BY-USER OUTPUT RECORD              */         
* /*             DVGQAR   - QUERY-BY-ADMINISTRATOR OUTPUT RECORD     */         
* /*                                                                 */         
* /*      AREAS TO BE PROVIDED IN THE ASSEMBLER PROGRAM:             */         
* /*                      - Query Response Area (QRA)                */         
* /*                      - Message area                             */         
* /*                      - APX area                                 */         
* /*                                                                 */         
* /*      MACROS:                                                    */         
* /*             DVGCALL   - INVOCATION OF DVGIFAI                   */         
* /*                                                                 */         
* /* $M0=DVGSAM,HFP3102,,KLZ: ASM SAMPLE FOR APPL. PROGRAM INTERFACE */         
* /*                                                                 */         
* /***END OF SPECIFICATIONS*******************************************/         
*                                                                               
R0       EQU   0                                                                
R1       EQU   1                                                                
R2       EQU   2                                                                
R3       EQU   3                                                                
R4       EQU   4                                                                
R5       EQU   5                                                                
R6       EQU   6                                                                
R7       EQU   7                                                                
R8       EQU   8                                                                
R9       EQU   9                                                                
R10      EQU   10                                                               
R11      EQU   11                                                               
R12      EQU   12                                                               
R13      EQU   13                                                               
R14      EQU   14                                                               
R15      EQU   15                                                               
         SAVE (14,12),,*                                                        
         BASR  9,0                                                              
         USING HERE,9,10,11      Establish three base registers                 
HERE     LM    R10,R11,BASES                                                    
         B     ADDRDUM                                                          
BASES    DC    A(HERE+4096,HERE+8192)                                           
********************************* ADDRESSABILITY FOR DSECT                      
ADDRDUM  EQU *                                                                  
         USING DVGQSR,R12         R12 TO ADDRESS QSR                            
         USING DVGAPX,R7          R7 TO ADDRESS APX                             
         USING APXSENT,R8         R8 TO ADDRESS APX SELECTION ENTRY             
********************************* SETUP SAVE-AREA                               
         ST 13,SAVE+4                                                           
         LA 5,SAVE                                                              
         ST 5,8(13)                                                             
         LR 13,5                                                                
*********************                                                           
*/*********************************************************************         
*/* MAIN PROCESSING              */                                             
*/********************************/                                             
*                                                                               
         OPEN (OUTDCB,(OUTPUT))                                                 
         LA    R6,DSYSIN        GET DCB ADDRESS                                 
         USING IHADCB,R6        MAP THE DCB                                     
         LA    R5,FILEEND       GET ADDRESS OF EOD ROUTINE                      
         STCM  R5,7,DCBEODA     MODIFY DCB WITH EOD ROUTINE                     
         B     OPENOK                                                           
         OPEN  ((6),INPUT)      OPEN THE FILE TO BE SENT                        
         TM    DCBOFLGS,DCBOFOPN  TEST OPEN OK                                  
         BO    OPENOK                                                           
         DROP  R6                                                               
         ABEND 10               OPEN NO GOOD                                    
OPENOK   EQU   *                                                                
         B     APIINIT            CALLS SUBROUTINE TO SET PARMS.                
ADDLAB   EQU   *                                                                
         B     APIADD             CALLS SUBR. FOR ADD REQUEST                   
QURLAB   EQU   *                                                                
         B     APIQUERY           CALLS SUBR. FOR QUERY REQUEST                 
DELLAB   EQU   *                                                                
***      B     APIDEL             CALLS SUBR. FOR DELETE REQUEST                
ENDLAB   CLOSE (OUTDCB)          Close output dataset                           
***      CLOSE SNAPDCB           close snap dcb for tests                       
         XR  15,15                                                              
         LA  13,SAVE                                                            
         L   13,4(13)                                                           
         L 15,APLRC                                                             
         RETURN (14,12),RC=(15)                                                 
*                                                                               
*/*********************************************************************         
*/* SUBROUTINES                  */                                             
*/********************************/                                             
*                                                                               
*/********************************/                                             
*/* INITIALISATION               */                                             
*/********************************/                                             
* ------------------------------------------------------------------- *         
BLANKIT  MVC   0(0,R8),0(R7)      Blank consecutive positions                   
* ------------------------------- Clear complete APL first ---------- *         
APIINIT  MVI   APL,C' '           Put a blank in the first position             
         LA    R7,APL             FROM position (already blank)                 
         LA    R8,APL+1           TO position for move                          
         LA    R5,APLLEN-1        Load EQU length of APL (1. position           
*                                   already cleared)                            
         LR    R6,R5              Total length left to be done in R6            
*                                                                               
         C     R6,=F'256'         Is total length left > 256 bytes?             
         BH    BLANK4             Branch if yes                                 
*                                                                               
BLANK2   S     R5,=F'1'           Length for EXECUTE is one less                
         EX    R5,BLANKIT         Blank out over a length as set in R5          
         A     R5,=F'1'           Reset to actual length again                  
         SR    R6,R5              Set new length left in R6                     
         BZ    APICONT            Leave if nothing left to move                 
*                                                                               
         LA    R7,256(R7)         Shift FROM position to the right              
         LA    R8,256(R8)         Shift TO   position to the right              
         C     R6,=F'256'         Still something left:                         
         BNH   BLANK3             - Now at the most 256 bytes.                  
BLANK4   L     R5,=F'256'         - Again more than 256 bytes;                  
         B     BLANK2               Re-execute with max. length                 
*                                                                               
BLANK3   LR    R5,R6              Set final length for execution                
         B     BLANK2             Re-execute with final length                  
* ------------------------------------------------------------------- *         
* After setting the whole APL to blanks, initialize the numeric fields*         
* to zeros.                                                           *         
APICONT  MVC   APLRC,ZEROCF       Return code                                   
         MVC   APLRSN,ZEROCF      Reason code                                   
         MVC   APLQACNT,ZEROCF    Number of query records in QRA                
         MVC   APLQTCNT,ZEROCF    Number of query records retrieved             
         MVC   APLMACT,ZEROCF     Number of messages in msg area                
         MVC   APLMTOT,ZEROCF     Number of messages issued in total            
         MVC   APLRQNUM,ZEROCF    Request number                                
         MVC   APLSDYBL,ZEROCF    Physical blocksize                            
         MVC   APLSDYLR,ZEROCF    Logical record length                         
         MVC   APLSDSS,ZEROCF     Send. dataset sequence number                 
         MVC   APLRVOC,ZEROCF     Rec. volume count                             
         MVC   APLRDSS,ZEROCF     Rec. dataset sequence number                  
         MVC   APLRDYBL,ZEROCF    Rec. physical blocksize                       
         MVC   APLRDYLR,ZEROCF    REC. logical record length                    
         MVC   APLRSPRM,ZEROCF    Primary space                                 
         MVC   APLRSSEC,ZEROCF    Secondary space                               
         MVC   APLRDIRB,ZEROCF    Directory blocks                              
         MVC   APLRAVBL,ZEROCF    Average blocklength                           
         MVC   APLPTRCP,ZEROCF    Trace area                                    
         MVC   APLPTRCL,ZEROCF    Length of trace area                          
         MVC   APLPNXRU,ZEROCF    Number of RUs transferred                     
         MVC   APLPNXR,ZEROCF     Number of records transferred                 
         MVC   APLPRUSZ,ZEROCF    RU size                                       
         MVC   APLAPXPT,ZEROCF    Pointer to APX PDS section                    
         MVC   APLSMBR#,ZEROCF    Max. # of selected PDS members                
         MVC   APLXMBR#,ZEROCF    Max. # of excluded PDS members                
         MVC   APLDYRSA,ZEROCF    Average record size                           
         MVC   APLDYKL,ZEROCF     Key length                                    
         MVC   APLDYKO,ZEROCF     Key offset                                    
         MVC   APLQSTAT,ZEROCF    Status parameter                              
         MVC   APLRDYLS,ZEROCF    SAM logical record size                       
         MVC   APLDELAY,ZEROCF    VSE request delay time                        
         MVC   APLSPPTR,ZEROCF    Post-transfer program area pointer            
         MVC   APLSPLEN,ZEROCF    Length of above area                          
         MVC   APLR4NRC,ZEROCF    Rec. init. # of records                       
         MVC   APLR4MRC,ZEROCF    Rec. max. # of records                        
         MVC   APLR4MXM,ZEROCF    Rec. max. member per file                     
         MVC   APLR4IRC,ZEROCF    Rec. incr. # of records                       
         MVC   APLR4MXI,ZEROCF    Rec. maximum increments                       
         MVC   APLRPPTR,ZEROCF    Post-transfer program area pointer            
         MVC   APLRPLEN,ZEROCF    Length of above area                          
         MVC   APLUXPTR,ZEROCF    Pointer to user exit area                     
         MVC   APLUXLEN,ZEROCF    Length of above area                          
         MVC   APLLXDPT,ZEROCF    Local user exit commun. data                  
         MVC   APLLXDLN,ZEROCF    Length of aboce data                          
         MVC   APLGXDPT,ZEROCF    Global user exit commun. data                 
         MVC   APLGXDLN,ZEROCF    Length of above data                          
         MVC   APLOIKIL,ZEROCF    OSI maximum string length                     
         MVC   APLOISFS,ZEROCF    OSI future file size                          
         MVC   APLOIPWL,ZEROCF    OSI password lengths                          
* ------------------------------------------------------------------ *          
         MVC  APLID,APLACCR            Insert EYE-CATCHER                       
         LA   4,APLLEN                 Get APL length als EQU                   
         STH  4,APLLNGTH               Store length in APL                      
         MVC  APLVID(3),APLVIDC        Insert APL version                       
*                                                                               
         LA   4,MSGSPACE               Set  pointer to MSG area                 
         ST   4,APLMAPTR                                                        
         LA   4,FBASPACE               Set  pointer to feedback area            
         ST   4,APLFBAP                                                         
         L    2,=F'1024'               Set  length of MSG area                  
         ST   2,APLMSGLN                                                        
         L    2,=F'133'                Set  length of feedback area             
         ST   2,APLFBALN                                                        
*                                      APX NEEDED FOR PO MEMBERS                
*        LA R7,APXSPACE                ADDRESSABILITY OF APX                    
*        LA R8,APXSPACE+16            ADDRESSABILITY OF SELECTION ENTRY         
*        LA R2,16                      LENGTH OF APX HEADER                     
*        ST R2,APXLEN                                                           
*        MVC APXID(8),APXACCR          SET APIX EYE-CATCHER                     
*                                                                               
***      OPEN (SNAPDCB,(OUTPUT))       open snap dcb for tests                  
*                                                                               
         B    ADDLAB                   Return to mainline                       
*                                                                               
APIADD   EQU *                                                                  
*/********************************/                                             
*/*  ADD REQUEST  function  see chapter 6 in user's manual                      
*/********************************/                                             
GETAGAIN EQU *                                                                  
         MVI  APLVBC,APLADDRE              FUNCTION   ADD                       
         MVC APLCLASS(1),=C'W'    server  class=w for western                   
*nop     MVC APLCLASS(1),=C'D'    server  class=d for dmb&b                     
         MVC APLPRTY(1),=C'1'             prty=1                                
*nop     MVC APLQSTAT(2),=H'64'           QSTAT=HOLD     ???                    
         MVC APLAPUID(7),=C'DDSUSER'      APPC CONV USER-ID AT DMB&B            
         MVC APLAPPWD(7),=C'DDSUSER'      AND PASSWORD  for western             
** TRANSFER PARAMETERS FOLLOW             PAGE 80                               
         MVI APLXMODE,APLTO               WE ARE A SENDER                       
         MVI APLSSMOD,APLCONT             CONTINUOUS RUNNING SERVERS            
***      MVC APLRLUNM(8),=C'WILA000T'     REMOTE LU NAME --> W.I.               
         MVC APLRLUNM(8),=C'WIL1500I'    per cliff collins      .               
         MVI APLREST,APLYES               RESTART FROM CHECKPOINT               
         MVI APLREQUE,APLYES              AUTOMATIC TRANSFER RESTART            
         MVC APLSNUID(4),=C'CCOL'         REPORT USER-ID                        
         MVC APLSNNID(4),=C'DDNMVS'       REPORT NODE-ID                        
         MVI APLRRCHK,APLNO               INDICATE REMOTE CHECK                 
*  FILE PARAMETERS          PAGE 94                                             
         MVC   APLSDYFD(L'MVSDSN),MVSDSN                                        
         PUT   OUTDCB,INBUFF           LOG THIS TO SYSOUT                       
*  SECURITY PARAMETERS                    PG 103                                
         TIME  DEC              ANSWER IN REG0 PACKED DECIMAL                   
         ST    R0,STARTIME         SAVE TIME                                    
         STCM  R1,X'7',STARTIME+4  SAVE DATE                                    
         UNPK  DATETIME(13),STARTIME(7)                                         
         MVC   A400TIME(6),DTHR                                                 
*  FILE PARAMETERS FOR OS/400 SYSTEM      PG 136                                
*  SEE APPENDIX D, PAGE 222 FOR SYSTEM GENERATED NAMES                          
*                                                                               
*        MVI APLR4OTY,APLDTAF             OS/400 FILE TYPE                      
*        MVI APLRDYTD,APLNEW              FILE OPTION == NEW                    
*        MVI APLR4MOP,APLADD              MEMBER OPTION, ADD                    
*  os/400 size group                                                            
         MVC APLR4MRC,=F'-1'              MAX RECORDS                           
*                                                                               
****                            snap dump for tests                             
***      LA R3,APXSPACE+1024                                                    
***      SNAP DCB=SNAPDCB,STORAGE=((R7),(R3)),PDATA=REGS                        
****                                                                            
         PRINT  GEN                                                             
         DVGCALL PARM=DVGAPLP          Sets up API interface                    
         CL   15,ZEROCF                Checks API return code                   
         BNE  APIERROR                                                          
         MVC  RQNUMVF,APLRQNUM         Saves request number                     
*                                      Prints return and reason codes           
         MVC  VERBCVC,APLVBC              Sets verb code                        
         L    2,APLRC                     Sets return code                      
         CVD  2,WRKVC                                                           
         UNPK RETRNVC,WRKVC                                                     
         MVZ  RETRNVC+3(1),HEXF0CF                                              
         L    2,APLRSN                    Sets reason code                      
         CVD  2,WRKVC                                                           
         UNPK REASNVC,WRKVC                                                     
         MVZ  REASNVC+3(1),HEXF0CF                                              
         PUT  OUTDCB,CODES                                                      
*NOP     B    GETAGAIN                 GO FOR NEXT RECORD                       
FILEEND  B    QURLAB                   GOES BACK TO MAINLINE                    
*                                                                               
*/********************************/                                             
*/*  QUERY REQUEST               */                                             
*/********************************/                                             
APIQUERY EQU *                                                                  
         B    DELLAB                   Leave                                    
*                                                                               
*/********************************/                                             
*/*  DELETE REQUEST              */                                             
*/********************************/                                             
APIDEL   MVI  APLVBC,APLDELSR          Sets verb code for DELETE                
         B    ENDLAB                   Goes back to mainline                    
*                                                                               
*/********************************/                                             
*/*  ERROR HANDLING              */                                             
*/********************************/                                             
APIERROR PUT  OUTDCB,MESSCC            Prints text (ERROR DVGAPI)               
*                                      Prints return and reason codes           
         MVC  VERBCVC,APLVBC              Sets verb code                        
         L    2,APLRC                     Sets return code                      
         CVD  2,WRKVC                                                           
         UNPK RETRNVC,WRKVC                                                     
         MVZ  RETRNVC+3(1),HEXF0CF                                              
         L    2,APLRSN                    Sets reason code                      
         CVD  2,WRKVC                                                           
         UNPK REASNVC,WRKVC                                                     
         MVZ  REASNVC+3(1),HEXF0CF                                              
         PUT  OUTDCB,CODES                                                      
*                                                                               
         CLC  APLMTOT,ZEROCF           Prints FTP message if available          
         BE   NOMSGLAB                                                          
*                                      Prints numbers of actual and             
*                                      total messages                           
         L    2,APLMACT                   Sets actual messages                  
         CVD  2,WRKVC                                                           
         UNPK ACTUAVC,WRKVC                                                     
         MVZ  ACTUAVC+3(1),HEXF0CF                                              
         L    2,APLMTOT                   Sets total messages                   
         CVD  2,WRKVC                                                           
         UNPK TOTALVC,WRKVC                                                     
         MVZ  TOTALVC+3(1),HEXF0CF                                              
         PUT  OUTDCB,MESACTO              Prints                                
         PUT  OUTDCB,MSGSPACE          Prints FTP message                       
NOMSGLAB B    ENDLAB                   Goes back to mainline                    
*                                                                               
*/*********************************************************************         
*   DECLARATIONS                 */                                             
**********************************/                                             
*                                                                               
*/*-----------------------------------------------------------------*/          
*/*- Definition for SYSPRINT                                       -*/          
*/*-----------------------------------------------------------------*/          
OUTDCB   DCB DDNAME=OUTDD,MACRF=(PM),DSORG=PS,RECFM=FB,LRECL=133,      /        
               BLKSIZE=133                                                      
SNAPDCB  DCB DSORG=PS,DDNAME=TSNAP,RECFM=VBA,MACRF=(W),                X        
               LRECL=125,BLKSIZE=1632                                           
*                                                                               
DSYSIN   DCB   DDNAME=SYSIN,DSORG=PS,MACRF=GM,RECFM=FB,LRECL=121                
*/*-----------------------------------------------------------------*/          
*/*- Definitions for constants                                     -*/          
*/*-----------------------------------------------------------------*/          
MVSDSN   DC    C'DDSFTP.CCOL.SBT.XXXX.BWI'     western billing tape             
         DS  0D                                                                 
SAVE     DS  18F                Save area                                       
ZEROCF   DC  X'00000000'                                                        
HEXF0CF  DC  X'F0'                                                              
         DS  CL3                                                                
MESSCC   DC  CL133'ERROR FROM API INTERFACE' Message text                       
APXACCR  DC  C'*DVGAPX*'       EYE CATCHER FOR APX                              
*                                                                               
*/*-----------------------------------------------------------------*/          
*/*- Definitions for variables                                     -*/          
*/*-----------------------------------------------------------------*/          
MATCHVB  DS  F                  Loop control switch                             
RQNUMVF  DS  F                  Request number                                  
WRKVC    DS  D                  Work area                                       
*                                                                               
*--------------------------------------------------------------------*/         
*-   Definitions for message and query response area                -*/         
*--------------------------------------------------------------------*/         
FBASPACE DS  CL133              Query response area                             
MSGSPACE DS  CL1024             Message area                                    
APXSPACE DS  CL1024             APX space                                       
*                                                                               
*/*-----------------------------------------------------------------*/          
*/*- Definitions of output structures                              -*/          
*/*-----------------------------------------------------------------*/          
CODES    DS  0CL133             Structure for return information after          
VERBCCC  DC  CL11'VERB CODE: '     CALL of the API                              
VERBCVC  DS  C                                                                  
RETRNCC  DC  CL16'  ,RETURN CODE: '                                             
RETRNVC  DS  F                                                                  
REASNCC  DC  CL16'  ,REASON CODE: '                                             
REASNVC  DS  F                                                                  
         DS  CL81                                                               
*                                                                               
MESACTO  DS  0CL133             Structure for return information in             
ACTUACC  DC  CL12'ACTUAL MSGS:'    case of an interface error                   
ACTUAVC  DS  F                                                                  
TOTALCC  DC  CL12',TOTAL MSGS:'                                                 
TOTALVC  DS  F                                                                  
         DS  CL101                                                              
*                                                                               
STARTIME DS    1F        TIME MACRO RESULT                                      
         DS    XL3       DATE                                                   
DATETIME DS    0CL13     UNPACKED TIME & DATE                                   
DTHR     DS    CL2       HOUR                                                   
DTMN     DS    CL2       MIN                                                    
DTSC     DS    CL2       SEC                                                    
         DS    CL2       WHO KNOWS                                              
DTYR     DS    CL2       YEAR                                                   
DTDAY    DS    CL3       DAY JULIAN                                             
A400FILE DC    CL3'DDS'                                                         
A400TIME DC    CL6'      '                                                      
*/*-----------------------------------------------------------------*/          
*/*- Definitions for DVGAPL and DVGQSR                             -*/          
*/*-----------------------------------------------------------------*/          
DVGAPLP  DC   A(DVGAPL)         Parameter for DVGCALL                           
         DVGAPL                 DVGAPL as CSECT                                 
         LTORG                                                                  
INBUFF   DS   CL2                                                               
INDSN    DS   CL33           rc=24 reason 435 dsn not found                     
INREFNO  ORG  INDSN+24                                                          
         DS   CL100                                                             
         DC   CL20'FTPFTP'                                                      
         DVGQSR DSECT=YES       DVGQSR as DSECT                                 
         DVGAPX DSECT=YES       DVGAPX as DSECT                                 
         DCBD  DSORG=PS,DEVD=DA                                                 
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007FRFTSBTBWI05/01/02'                                      
         END                                                                    
