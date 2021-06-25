*          DATA SET CTMAD24    AT LEVEL 096 AS OF 07/20/20                      
*PHASE TA0C24A                                                                  
*INCLUDE REFORM                                                                 
*INCLUDE GETBOOK                                                                
         TITLE 'TA0C24 - $MAD SQL FORMAT REPORT DOWNLOAD'                       
TA0C24   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C24,RA,RR=R4                                                
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
         L     R6,ATMPBUF          R6 = TEMPORARY STORAGE BUFFER                
         USING TEMPD,R6                                                         
*                                                                               
         L     R1,=V(REFORM)       RELOCATE EXTERNALS                           
         AR    R1,R4                                                            
         ST    R1,VREFORM                                                       
         L     R1,=A(REFAREA-TEMPD)                                             
         AR    R1,R6                                                            
         ST    R1,AREFAREA                                                      
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
*************************************************************                   
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE     *                   
* THE THREE MODES (START, MIDDLE AND END.)                  *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
* THIS ROUTINE INITIALIZES VARIABLES                        *                   
*************************************************************                   
         SPACE 1                                                                
INIT     NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
IN10     MVI   FLREAD,C'N'         SET FIRST PLINE OF TRANS NOT READ            
*                                                                               
         GOTO1 INITBUF             INITIALIZE PQ BUFFER                         
         BNE   EXIT                                                             
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
* THIS ROUTINE PROCESSES THE START MODE.                    *                   
* IT VALIDATES THE PQ ID PASSED BY THE PC.                  *                   
* IT THEN RETURNS A DETAILED REPORT SUMMARY OBJECT          *                   
* FOLLOWED BY THE FIRST FRAME OF DATA.                      *                   
*************************************************************                   
         SPACE 1                                                                
PROCSTRT NTR1                                                                   
         GOTO1 VALSPID             VALIDATE SPOOL ID                            
         BNE   EXIT                                                             
*                                  VALIDATE REPORT                              
         GOTO1 GETNDX,DMCB,SIGNON2,REPSUBID,REPREPNO                            
         BE    PS10                                                             
         CLI   PQEOF,C'Y'          EOF - REPORT NOT FOUND                       
         BE    ERRRNF                                                           
         B     EXIT                DISK ERROR                                   
*                                                                               
PS10     GOTO1 GETATTR,DMCB,0      SAVE REPORT ATTRIBUTES                       
         BNE   EXIT                                                             
*                                                                               
         LA    R5,INQUIRY          IF REPORT HAS JCL IN IT THEN RETURN          
         USING INQUIRYD,R5         ERROR TO HOST                                
         GOTO1 HEXIN,DMCB,IQATTR,BYTE,2                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRJCL                                                           
         TM    BYTE,PQATJOBI                                                    
         BO    ERRJCL                                                           
         TM    BYTE,PQATERR                                                     
         BO    ERRERROR                                                         
         GOTO1 HEXIN,DMCB,IQSTAT,BYTE,2                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRJCL                                                           
*                                                                               
         MVC   SQREFID,IQDESC+6                                                 
         DROP  R5                                                               
*                                                                               
         GOTO1 PUTITEM,DMCB,ITDETINQ,IQDETLEN,INQUIRY                           
         BNE   EXIT                                                             
*                                                                               
         GOTO1 RNDPAGE,DMCB,1      READ FIRST PAGE OF REPORT                    
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,BLDBUFF          BUILD BUFFER OF REQ DETAILS                  
*                                                                               
         LA    RE,REFBLOCK         CLEAR REFBLOCK                               
         LH    RF,=Y(L'REFBLOCK)                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,REFBLOCK                                                      
         USING REFORMD,R2                                                       
         OI    REFSFLG1,REFSINIQ                                                
         MVC   REFID,SQREFID                                                    
         L     R1,AUTL                                                          
         MVC   REFAGY,TAGY-UTLD(R1)                                             
*                                                                               
         GOTO1 VREFORM,DMCB,REFBLOCK,REQBUFF,ACOMFACS                           
         TM    REFSFLG1,REFSERRQ                                                
         BNZ   ERROUT                                                           
*                                                                               
         BAS   RE,FILLFRM          FILL FRAME WITH REPORT DATA OBJECTS          
*                                                                               
         GOTO1 WRTTWA,DMCB,ATMPBUF,3                                            
*                                                                               
         GOTO1 MARKREP,DMCB,0      MARK REPORT IS DOWNLOADING                   
         BNE   EXIT                                                             
*                                  PUT DETAIL INQUIRY OBJECT                    
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
* MIDDLE MODE.  RETURNS THE NEXT FRAME FULL                 *                   
* AND SETS LAST FRAME FLAG IF IT REACHES THE END            *                   
*************************************************************                   
         SPACE 1                                                                
PROCMID  NTR1                                                                   
*                                                                               
         GOTO1 READTWA,DMCB,ATMPBUF,3                                           
*                                                                               
         BAS   RE,FILLFRM          FILL FRAME WITH REPORT DATA OBJECTS          
*                                                                               
         GOTO1 WRTTWA,DMCB,ATMPBUF,3                                            
*                                                                               
MMX      B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
* END MODE. SETS THE SENT FLAG IN THE REPORT HEADER         *                   
*************************************************************                   
         SPACE 1                                                                
PROCEND  NTR1                                                                   
         GOTO1 MARKREP,DMCB,1      MARK REPORT IS SENT                          
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* MARKS THE REPORT FOR DOWNLOADING                                              
*        P1 = 1 (MARK IT SENT)  P1=0 (MARK IT SENDING)                          
***********************************************************************         
MARKREP  NTR1                                                                   
         CLC   PCACTION,=Y(ACDISREP)  NO STATUS CHANGE FOR DISREP               
         BE    XIT                                                              
         USING PQRECD,R5           R5=PQ REPORT DSECT                           
         MVC   BYTE,3(R1)                                                       
         LA    RE,L'PQINDEX        SET INDEX LENGTH                             
         STH   RE,CINDXLN                                                       
         L     RF,ASYSFACS                                                      
         MVC   CIENQDEQ,VENQDEQ-SYSFACD(RF)                                     
*                                                                               
         L     R5,ATIA             INIT PQ BUFFER                               
         GOTO1 DATAMGR,DMCB,(X'00',BUFFER),PRTQID,NDX,PQLINE,(R5),0             
         CLI   8(R1),0                                                          
         BNE   MRERROR                                                          
         MVC   CIDATA,12(R5)       SET CIDATA                                   
*                                                                               
         SAM31                                                                  
         MVC   FIWRES,PRTQID                                                    
         BRAS  RE,FIRSET                                                        
*                                                                               
         MVC   CIADDR,PSAVEB+SKFSTCI-SKBUFFD                                    
         MVC   FIWCIA,CIADDR                                                    
         BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE) AND REPORT#         
         BRAS  RE,FIRN1                                                         
         BNE   MRERROR             ERROR: NOT A VALID REPORT#                   
         BRAS  RE,FIRSLOCK         SHARED LOCK OF PRINT QUEUE                   
         BRAS  RE,FIRRLOCK         LOCK PRINT QUEUE REPORT                      
         SAM24                                                                  
*                                                                               
         CLI   BYTE,0              IS THIS A PRINTING REQUEST                   
         BE    MR020                                                            
*                                                                               
         TBIN  SECS                GET DATE TIME NOW                            
         SR    R0,R0                                                            
         D     R0,=F'60'           GET MINUTES                                  
         SR    R0,R0                                                            
         D     R0,=F'60'           GET HOURS                                    
         STC   R1,TIMEB                                                         
         STC   R0,TIMEB+1                                                       
         GOTO1 DATCON,DMCB,(5,0),(30,DATEC)                                     
         GOTO1 (RF),(R1),,(3,DATEB)                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),PRTQID,CIADDR,ATIA                   
         CLI   8(R1),0                                                          
         BNE   MRERRUR                                                          
         MVC   PQDATED,DATEC       SET DATE/TIME PRINTED                        
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    MR001                                                            
         GOTO1 DATCON,DMCB,(14,PQDATED),(2,PQDATED)                             
MR001    MVC   PQAGEDD,DATEC       SET DATE/TIME PRINTED                        
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    MR002                                                            
         GOTO1 DATCON,DMCB,(14,PQAGEDD),(2,PQAGEDD)                             
MR002    MVC   PQTIMED,TIMEB                                                    
         L     RE,AUTL                                                          
         MVC   PQPRSYM,TSYM-UTLD(RE)    SET LOCATION FROM UTL                   
         IC    RE,PQPRCNT          BUMP NUMBER OF TIMES SENT                    
         LA    RE,1(RE)                                                         
         STC   RE,PQPRCNT                                                       
         NI    PQSTAT,255-(PQSTAC+PQSTTE)                                       
         OI    PQSTAT,PQSTSE       SET REPORT TO SENT STATUS                    
*                                                                               
         LA    R1,WORK             USE GETRET FOR RETAIN INFO                   
         USING GETRETD,R1                                                       
         MVC   GRDHRS,PQRETND                                                   
         MVC   GRDIDY(3),DATEB                                                  
         MVC   GRDITH(2),TIMEB                                                  
         GOTO1 GETRET                                                           
         LA    R1,WORK                                                          
         SR    RF,RF               CALCULATE BINARY 10MINS RET TIME             
         SR    RE,RE                                                            
         IC    RF,GRDOTH                                                        
         IC    RE,GRDOTM                                                        
         MH    RF,=H'60'           CALCULATE MINUTES                            
         AR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         STC   RF,DUB              SAVE IN DUB                                  
         MVC   FULL,GRDODY                                                      
         GOTO1 DATCON,DMCB,(3,FULL),(30,HALF)                                   
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      TEST NEW/OLD CMPRSD DATE                     
         BO    *+8                                                              
         MVI   DMCB+4,2            USE OLD TYPE 2                               
         BASR  RE,RF                                                            
         MVC   PQAGERD,HALF                                                     
         MVC   PQAGERT,DUB         SET RETAIN DATE/TIME                         
         DROP  R1                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'00',DMWRT),PRTQID,CIADDR,ATIA                    
         CLI   8(R1),0                                                          
         BNE   MRERRUR                                                          
*                                  READ INDEX PAGE                              
MR020    SAM31                                                                  
         L     R5,FIWNDA           POINT TO INDEX NODE IN MEMORY                
         LA    R5,SI1NDX-SI1PAR(R5) R5=A(INDEX)                                 
*                                                                               
         CLI   BYTE,1              IS THIS A SENT REQUEST                       
         BE    MR30                                                             
*                                                                               
         OI    PQSTAT-PQINDEX(R5),PQSTPG                                        
*                                                                               
         L     RF,ASYSFACS         RF= A(SYSFACS)                               
         L     RF,VSSB-SYSFACD(RF) RF= A(SSB)                                   
         L     RE,AUTL             RE=AUTL                                      
         MVC   PQAGEDD(2),TNUM-UTLD(RE)                                         
         OC    PQAGEDD(1),SSBSYSID-SSBD(RF)                                     
         MVI   PQAGERT,X'FE'       SET REPORT PRINTING                          
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         B     MR40                                                             
*                                                                               
MR30     NI    PQSTAT-PQINDEX(R5),255-(PQSTTE+PQSTAC)                           
         OI    PQSTAT-PQINDEX(R5),PQSTSE                                        
         MVC   PQAGERD,HALF                                                     
         MVC   PQAGERT,DUB         SET NEW RETAIN INFO                          
*                                                                               
MR40     B     MREXIT                                                           
*                                                                               
MRERRUR  BRAS  RE,FIRRUNLK         UNLOCK PRINT QUEUE REPORT                    
         BRAS  RE,FIRSUNLK         UNLOCK SHARED PRINT QUEUE                    
MRERROR  B     ERRMRKRP                                                         
*                                                                               
MREXIT   BRAS  RE,FIRRUNLK         UNLOCK PRINT QUEUE REPORT                    
         BRAS  RE,FIRSUNLK         UNLOCK SHARED PRINT QUEUE                    
         CR    RB,RB               SET CC=EQU                                   
         B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* FILL FRAME WITH MAD OBJECTS                                                   
***********************************************************************         
FILLFRM  NTR1                                                                   
*                                                                               
         LA    R2,REFBLOCK         R2=REFBLOCK                                  
         USING REFORMD,R2                                                       
         TM    REFSFLG1,REFSCBAQ   TEST FOR CALLBACK FROM PREVIOUS              
         BO    FF21                                                             
         MVI   REFSFLG1,REFSDATQ   SET DATA LINE INPUT FLAG                     
*                                                                               
FF10     CLI   FLREAD,C'Y'         TEST PQ READS INITIALISED                    
         BE    FF11                                                             
         GOTO1 FIRSTLIN            FIRST LINE                                   
         MVI   FLREAD,C'Y'                                                      
*                                                                               
FF11     CLI   PQEOF,C'Y'          IF END OF REPORT THEN PUT EOD ITEM           
         BE    FF50                                                             
*                                                                               
         MVI   REFSFLG1,REFSDATQ   SET DATA LINE INPUT FLAG                     
         XC    REFDATA,REFDATA                                                  
         ICM   R1,15,PQLEN         EX LEN IS PQLEN -1                           
         SH    R1,=H'1'                                                         
         BM    FF20                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REFDATA(0),PQPLIN+1                                              
FF20     MVC   REFDLEN,PQLEN       DATA LEN IS PQLEN                            
*                                                                               
FF21     GOTO1 VREFORM,DMCB,REFBLOCK,AREFAREA,ACOMFACS                          
         TM    REFSFLG1,REFSERRQ                                                
         BZ    *+6                                                              
         DC    H'0'                ERROR RETURN FROM REFORM                     
*                                                                               
         TM    REFSFLG1,REFSOUTQ   ANY OUTPUT DATA TO PROCESS                   
         BZ    FF30                                                             
*                                                                               
         L     R3,AREFAREA         PUT TO FRAME                                 
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         SH    RF,=H'4'                                                         
         LA    R3,4(R3)                                                         
         GOTO1 PUTITEM,DMCB,ITSTRING,(RF),(R3)                                  
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME REACHED THEN DONE            
         BE    FFX                                                              
*                                                                               
         TM    REFSFLG1,REFSGETQ   DO WE WANT TO GET NEXT LINE                  
         BZ    FF21                                                             
*                                                                               
FF30     CLI   FLREAD,C'Y'         TEST PQ INITIALISED                          
         BNE   FF10                                                             
         GOTO1 NEXTLIN             GET NEXT PQ LINE                             
         CLI   PQEOF,C'Y'          IF END OF REPORT THEN DONE                   
         BE    FF50                                                             
         B     FF10                LOOP BACK                                    
*                                                                               
*                                  PUT END OF DATA ITEM                         
FF50     GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
         GOTO1 MARKREP,DMCB,1      MARK REPORT IS SENT                          
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME REACHED THEN DONE            
         BE    FFX                                                              
*                                                                               
         MVI   MDLAST,C'Y'         ELSE SET LAST FRAME FLAG TO 'Y'              
*                                                                               
FFX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
**************************************************************                  
*        BUILD BUFFER OF REQUEST DETAILS                     *                  
**************************************************************                  
BLDBUFF  NTR1                                                                   
*                                                                               
         LA    RE,REQBUFF          CLEAR REQBUFF                                
         LH    RF,=Y(L'REQBUFF)                                                 
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,=C' '                                                       
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,REQBUFF          R2=A(BUFFER)                                 
*                                                                               
         GOTO1 FIRSTLIN            FIRST LINE                                   
         MVI   FLREAD,C'Y'                                                      
*                                                                               
BF10     CLI   PQEOF,C'Y'          IF END OF REPORT THEN EXIT                   
         BE    BFX                                                              
*                                                                               
         ICM   R1,15,PQLEN         COPY LINE                                    
         SH    R1,=H'1'                                                         
         BM    BF20                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),PQPLIN+1                                                 
*                                                                               
BF20     LA    R2,1(R1,R2)         SET ZERO DELIMITER                           
         MVI   0(R2),0                                                          
         LA    R2,1(R2)                                                         
*                                                                               
         GOTO1 NEXTLIN             ELSE PARSE OUT NEXT ITEM                     
         CLC   PQPAGE,=F'1'                                                     
         BE    BF10                LOOP BACK                                    
*                                                                               
         MVI   0(R2),0             SET DOUBLE ZERO END                          
*                                                                               
BFX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* RETURN ERROR OBJECT                                                           
***********************************************************************         
         USING REFORMD,R2                                                       
ERROUT   MVC   APPLERR,REFSCODE+1                                               
         GOTO1 HEXOUT,DMCB,APPLERR,FULL,2                                       
         GOTO1 PUTITEM,DMCB,A(ITGENERR),4,FULL                                  
         BNE   EXIT                                                             
         MVI   MDLAST,C'Y'                                                      
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ERROR EXITS                                                                   
***********************************************************************         
ERRRNF   MVC   MDACTION,=Y(DLRNFERR)  REPORT NOT FOUND                          
         J     EXIT                                                             
ERRJCL   MVC   MDACTION,=Y(DLJCLREP)  REPORT CONTAINS JCL                       
         J     EXIT                                                             
ERRERROR MVC   MDACTION,=Y(DLREPERR)  REPORT IS IN ERROR                        
         J     EXIT                                                             
ERRMRKRP MVC   MDACTION,=Y(DLMRKREP)  DISK ERROR                                
         J     EXIT                                                             
*                                                                               
EXIT     L     RD,SAVEDRD             RESTORE RD                                
XIT      XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* CONSTANTS AND LTORG                                                           
***********************************************************************         
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
INDEX    DC    CL8'INDEX'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
READL    DC    CL8'READ'                                                        
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
PRTQUE   DC    CL8'PRTQUE'                                                      
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* SHARED MEMORY ROUTINES                                                        
***********************************************************************         
       ++INCLUDE SHFIR                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
OVERD    DSECT                                                                  
*                                                                               
*        MISCELLANEOUS VARIABLES                                                
*                                                                               
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
VREFORM  DS    A                                                                
AREFAREA DS    A                                                                
SVRE     DS    A                                                                
SVR4     DS    A                                                                
*                                                                               
*        VARIABLES SET IN INIT                                                  
*                                                                               
SQREFID  DS    CL5                                                              
FLREAD   DS    C                   FLAGS FIRST PRINT LINE HAS BEEN READ         
*                                                                               
*        VARIABLES USED BY ALL                                                  
*                                                                               
TIMEB    DS    XL2                                                              
DATEC    DS    XL2                 CMPRSD DATE NEW FORMAT                       
DATEB    DS    XL3                                                              
         DS    0H                                                               
*                                                                               
*        SHARED MEMORY DATA                                                     
*                                                                               
       ++INCLUDE SHFIW             SHARED MEMORY DATA                           
*                                                                               
*        CIDATA INFO, THIS IS THE PRE-DTF                                       
*                                                                               
       ++INCLUDE DMPRTQW           CIDATA INFO, THIS IS THE PRE-DTF             
         EJECT                                                                  
                                                                                
***********************************************************************         
* TEMPORY AREA DSECT                                                            
***********************************************************************         
TEMPD    DSECT                                                                  
TEMPAREA DS    0CL18432                                                         
REQBUFF  DS    CL(15*80)                                                        
REFBLOCK DS    CL(REFORML)                                                      
REFAREA  DS    1024C                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQS                                                        
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDGETRETD                                                      
         EJECT                                                                  
       ++INCLUDE SHFID                                                          
         EJECT                                                                  
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
                                                                                
* DDCOMFACS                                                                     
* CTGENFILE                                                                     
* DDREFORMD                                                                     
* IHAASCB                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDREFORMD                                                      
         IHAASCB LIST=YES                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096CTMAD24   07/20/20'                                      
         END                                                                    
