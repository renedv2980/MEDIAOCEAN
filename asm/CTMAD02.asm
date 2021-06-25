*          DATA SET CTMAD02    AT LEVEL 003 AS OF 07/20/20                      
*PHASE TA0C02A                                                                  
         TITLE 'TA0C02 - $MAD REPORTS INQUIRY'                                  
TA0C02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C02,RA                                                      
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER'S STORAGE)                 
         L     R9,AOVER            R9 = A(OVERLAY'S STORAGE)                    
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
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
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.                                                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
         GOTO1 INITBUF             INITIALIZE PQ BUFFER                         
         BNE   EXIT                                                             
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE START MODE.  IT VALIDATES THE INQUIRY              
* FILTER OBJECT PASSED BY ALINK AND RETURNS THE FIRST FRAME FULL OF             
* INQUIRY OBJECTS.                                                              
*                                                                               
PROCSTRT NTR1                                                                   
         BAS   RE,VALFILT          VALIDATE FILTER OBJECT                       
*                                                                               
         XC    NUMSKIP,NUMSKIP     SKIP ZERO REPORTS FOR FIRST FRAME            
*                                                                               
         BAS   RE,FILLFRM          FILL FIRST FRAME OF INQURIY OBJECTS          
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES MIDDLE MODE.  IT RETURNS THE NEXT FRAME FULL           
* OF REPORT DATA OBJECTS AND SETS THE LAST FRAME FLAG IF IT REACHES THE         
* END OF THE REPORT.                                                            
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
         BAS   RE,FILLFRM          FILL FRAME WITH INQUIRY OBJECTS              
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE GETS THE FILTER OBJECT FROM THE INPUT FRAME AND MAKES            
* SURE IT IS VALID.                                                             
*                                                                               
VALFILT  NTR1                                                                   
         GOTO1 GETITEM             GET FIRST ITEM - FILTER OBJECT               
         BNE   EXIT                                                             
*                                  MUST BE TYPE FILTER OBJECT                   
         CLC   TYPENUM,=A(ITINQFLT)                                             
         BNE   ERRFILT                                                          
*                                                                               
         CLC   DATALEN,=A(IFOLENQ) MUST BE OLD LENGTH                           
         BE    VF010                                                            
         CLC   DATALEN,=A(IFLENQ)  OR NEW LENGTH                                
         BE    VF010                                                            
         BNE   ERRFILT                                                          
*                                                                               
VF010    L     R4,ADATA            SAVE FILTER IN FILTER BLOCK                  
         MVC   INQFILT,0(R4)                                                    
         USING INQFILTD,R4                                                      
*                                  CONVERT OFFICE CODE TO 2 BYTE FORMAT         
         GOTO1 CONVOFF,DMCB,(1,IFOFFICE),HALF                                   
         BNE   ERROFF              ERROR OFFICE CODE NOT FOUND                  
*                                                                               
         L     RF,ATWA             IF OFFICE CODE DOESN'T MATCH TWA             
         CLC   HALF,10(RF)                                                      
         BNE   ERRMAT              THEN ERROR                                   
*                                                                               
*                                  VALIDATE REP ATTR POSITIVE FILTER            
         GOTO1 HEXIN,DMCB,IFATTRP,ATTRPOS,2                                     
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRFILT                                                          
*                                  VALIDATE REP ATTR NEGITIVE FILTER            
         GOTO1 HEXIN,DMCB,IFATTRN,ATTRNEG,2                                     
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRFILT                                                          
*                                  VALIDATE REP STAT POSITIVE FILTER            
         GOTO1 HEXIN,DMCB,IFSTATP,STATPOS,2                                     
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRFILT                                                          
*                                  VALIDATE REP STAT NEGITIVE FILTER            
         GOTO1 HEXIN,DMCB,IFSTATN,STATNEG,2                                     
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRFILT                                                          
*                                                                               
         CLC   DATALEN,=A(IFOLENQ) TEST OLD FILTER BLOCK                        
         BE    VFX                                                              
*                                                                               
*        VALIDATION OF SYS/PRG/SUB/SDATE GOES HERE!                             
*                                                                               
VFX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* THIS ROUTINE FILLS THE FRAME WITH REPORT INQUIRY OBJECTS.  IT SKIPS           
* THE FIRST 'NUMSKIP' NUMBER OF REPORTS TO GET TO THE FIRST REPORT.             
* UPON EXIT 'NUMSKIP' WILL HOLD THE NUMBER OF REPORTS TO SKIP TO GET TO         
* THE NEXT FRAME.                                                               
*                                                                               
FILLFRM  NTR1                                                                   
         LA    R6,INQUIRY          R6 = A(INQUIRY BLOCK)                        
         USING INQUIRYD,R6                                                      
*                                                                               
         XC    NDX,NDX             CLEAR INDEX KEY                              
*                                                                               
         OC    NUMSKIP,NUMSKIP     IF WE NEED TO SKIP REPORTS                   
         BZ    FF20                                                             
*                                                                               
         L     R4,NUMSKIP          THEN R4 = NUMBER OF REPORTS TO SKIP          
*                                                                               
*                                  GET NEXT REPORT INDEX                        
FF10     GOTO1 GETNDX,DMCB,(X'80',0)                                            
         BNE   FFERR                                                            
*                                                                               
         BCT   R4,FF10             REPEAT UNTIL NO MORE REPORTS TO SKIP         
*                                                                               
FF20     L     R4,NUMSKIP          R4 = NUM OF REPS TO SKIP NEXT TIME           
*                                                                               
*                                  GET NEXT REPORT INDEX                        
FF30     GOTO1 GETNDX,DMCB,(X'80',0)                                            
         BNE   FFERR                                                            
*                                                                               
         BAS   RE,FILTNDX          IF INDEX FILTERED THEN SKIP REPORT           
         BNE   FF90                                                             
*                                                                               
         GOTO1 GETATTR,DMCB,0      FILL INQUIRY BLOCK WITH REPORT ATTRS         
*                                                                               
         BAS   RE,FILTHDR          IF HEADER FILTERED THEN SKIP REPORT          
         BNE   FF90                                                             
*                                  COMPUTE INQUIRY ITEM'S TYPE & LEN            
         LA    R2,ITEXTINQ         R2 = ITEM TYPE                               
         LA    R3,IQEXTLEN         R3 = DATA LENGTH                             
         CLC   PCACTION,=Y(ACEXTINQ)                                            
         BE    FF40                                                             
         LA    R2,ITDETINQ         R2 = ITEM TYPE                               
         LA    R3,IQDETLEN         R3 = DATA LENGTH                             
         CLC   PCACTION,=Y(ACDETINQ)                                            
         BE    FF40                                                             
         LA    R2,ITTOTINQ                                                      
         LA    R3,IQTOTLEN                                                      
         CLC   PCACTION,=Y(ACTOTINQ)                                            
         BE    FF35                                                             
         LA    R2,ITSUMINQ                                                      
         LA    R3,IQSUMLEN                                                      
         B     FF40                                                             
*                                  PUT INQUIRY ITEM                             
FF35     SR    RF,RF               FIELD IS IQTOTLEN+10+STRDAT                  
         CLC   PCVRS,=H'330'       FOR VERSION 330                              
         BL    *+8                                                              
         ICM   RF,3,IQSTRLEN                                                    
         LA    RF,IQTOTLEN(RF)                                                  
         GOTO1 PUTITEM,DMCB,ITTOTINQ,(RF),INQUIRY                               
         BNE   EXIT                                                             
         B     FF45                                                             
*                                  PUT INQUIRY ITEM                             
FF40     GOTO1 PUTITEM,DMCB,(R2),(R3),INQUIRY                                   
         BNE   EXIT                                                             
*                                                                               
FF45     CLI   EOFFLAG,C'Y'        IF END OF FRAME THEN DONE                    
         BE    FF100                                                            
*                                                                               
FF90     LA    R4,1(R4)            INCREMENT NUMBER OF REPORTS TO SKIP          
         B     FF30                LOOP BACK                                    
*                                                                               
FF100    ST    R4,NUMSKIP          SAVE NEW NUMBER OF REPORTS TO SKIP           
         B     FFX                                                              
*                                                                               
FFERR    CLI   PQEOF,C'Y'          IF NOT EOF THEN DISK ERROR                   
         BNE   EXIT                                                             
*                                  ELSE PUT END OF DATA ITEM                    
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME THEN DONE                    
         BE    FF100                                                            
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
*                                                                               
FFX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE TESTS THE CONTENTS OF THE INQUIRY BLOCK AGAINST THE              
* INDEX FILTERS AND RETURNS 'YES' IF THE INQUIRY PASSES THE FILTERS             
* AND 'NO' OTHERWISE.                                                           
*                                                                               
FILTNDX  NTR1                                                                   
         LA    R5,INQFILT          R5 = A(INQUIRY FILTERS)                      
         USING INQFILTD,R5                                                      
         LA    R6,INQUIRY          R6 = A(INQUIRY BLOCK)                        
         USING INQUIRYD,R6                                                      
*                                                                               
         LA    R4,IFOFFICE         R4 = A(OFFICE FILTER)                        
         LA    R3,IQOFFICE         R3 = A(INQUIRY OFFICE)                       
         LA    R2,8                R2 = LENGTH OF COMPARE                       
*                                                                               
FN10     CLI   0(R4),C'*'          IF FILTER BYTE = '*' THEN SKIP               
         BE    FN20                                                             
         CLC   0(1,R4),0(R3)       ELSE COMPARE FILTER TO INQUIRY               
         BNE   FNNO                                                             
*                                                                               
FN20     LA    R4,1(R4)            BUMP TO NEXT FILTER BYTE                     
         LA    R3,1(R3)            BUMP TO NEXT INQUIRY BYTE                    
         BCT   R2,FN10             REPEAT UNTIL END OF OFFICE                   
*                                                                               
         LA    R4,IFSUBID          R4 = A(SUB-ID FILTER)                        
         LA    R3,IQSUBID          R3 = A(INQUIRY SUB-ID)                       
         LA    R2,3                R2 = LENGTH OF COMPARE                       
*                                                                               
FN30     CLI   0(R4),C'*'          IF FILTER BYTE = '*' THEN SKIP               
         BE    FN40                                                             
         CLC   0(1,R4),0(R3)       ELSE COMPARE FILTER TO INQUIRY               
         BNE   FNNO                                                             
*                                                                               
FN40     LA    R4,1(R4)            BUMP TO NEXT FILTER BYTE                     
         LA    R3,1(R3)            BUMP TO NEXT INQUIRY BYTE                    
         BCT   R2,FN30             REPEAT UNTIL END OF SUB-ID                   
*                                                                               
FN50     EQU   *                                                                
FN60     EQU   *                                                                
FN70     EQU   *                                                                
*                                                                               
FNYES    B     YES                                                              
*                                                                               
FNNO     B     NO                                                               
         EJECT                                                                  
* THIS ROUTINE TESTS THE CONTENTS OF THE INQUIRY BLOCK AGAINST THE              
* REPORT HEADER FILTERS AND RETURNS 'YES' IF THE INQUIRY PASSES THE             
* FILTERS AND 'NO' OTHERWISE.                                                   
*                                                                               
FILTHDR  NTR1                                                                   
         LA    R5,INQFILT          R5 = A(INQUIRY FILTERS)                      
         USING INQFILTD,R5                                                      
         LA    R6,INQUIRY          R6 = A(INQUIRY BLOCK)                        
         USING INQUIRYD,R6                                                      
*                                                                               
         CLI   ATTRPOS,0           IF ATTR POSITIVE FILTER IS NOT ZERO          
         BE    FH10                                                             
         ZIC   RF,ATTRPOS                                                       
         EX    RF,*+8              THEN SOME BIT SET IN POS FILTER              
         B     *+8                     MUST BE SET IN INQUIRY                   
         TM    REPATTR,0                                                        
         BZ    FHNO                                                             
*                                                                               
FH10     TM    REPATTR,PQATERR     NEVER SHOW ERROR REPORTS                     
         BO    FHNO                                                             
         CLI   ATTRNEG,0           IF ATTR NEGITIVE FILTER IS NOT ZERO          
         BE    FH20                                                             
         ZIC   RF,ATTRNEG                                                       
         EX    RF,*+8              THEN SOME BIT SET IN NEG FILTER              
         B     *+8                     MUST BE CLEAR IN INQUIRY                 
         TM    REPATTR,0                                                        
         BO    FHNO                                                             
*                                                                               
FH20     TM    REPSTAT,PQSTIN      IGNORE INVISIBLE REPORTS                     
         BO    FHNO                                                             
         CLI   STATPOS,0           IF STAT POSITIVE FILTER IS NOT ZERO          
         BE    FH30                                                             
         ZIC   RF,STATPOS                                                       
         EX    RF,*+8              THEN SOME BIT SET IN POS FILTER              
         B     *+8                     MUST BE SET IN INQUIRY                   
         TM    REPSTAT,0                                                        
         BZ    FHNO                                                             
*                                                                               
FH30     CLI   STATNEG,0           IF STAT NEGITIVE FILTER IS NOT ZERO          
         BE    FH40                                                             
         ZIC   RF,STATNEG                                                       
         EX    RF,*+8              THEN SOME BIT SET IN NEG FILTER              
         B     *+8                     MUST BE CLEAR IN INQUIRY                 
         TM    REPSTAT,0                                                        
         BO    FHNO                                                             
*                                                                               
FH40     CLI   IFFORMAT,C'*'       IF FORMAT FILTER = '*' THEN SKIP             
         BE    FH50                                                             
         CLC   IFFORMAT,IQFORMAT   ELSE INQUIRY MUST MATCH FILTER               
         BNE   FHNO                                                             
*                                                                               
FH50     GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         L     R4,ATIA                                                          
         USING PQRECD,R4                                                        
*                                                                               
         CLC   IFCREATE(4),=C'****'    IF **S THEN SKIP                         
         BE    FH60                                                             
*                                                                               
         MVC   WORK+2(4),IFCREATE                                               
         GOTO1 DATCON,DMCB,(14,PQDATEL),(0,BLOCK)                               
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF TO NEW CMPRSD DATE                  
         BO    *+8                                                              
         MVI   DMCB,2              OLD COMP                                     
         BASR  RE,RF                                                            
         CLC   WORK(6),BLOCK                                                    
         BH    FHNO                                                             
*                                                                               
FH60     CLC   IFSENT(4),=C'****'  IF **S THEN SKIP                             
         BE    FH65                                                             
         OC    IFSENT,IFSENT       IF ZERO THEN SKIP                            
         BE    FH65                                                             
         MVC   WORK+2(4),IFSENT                                                 
         GOTO1 DATCON,DMCB,(14,PQAGEDD),(0,BLOCK)                               
         ORG   *-2                                                              
         TM    PQTYP1,PQTYNCD      SET HALF TO NEW CMPRSD DATE                  
         BO    *+8                                                              
         MVI   DMCB,2              OLD COMP                                     
         BASR  RE,RF                                                            
         CLC   WORK(6),BLOCK                                                    
         BH    FHNO                                                             
*                                                                               
FH65     CLI   IFSYS,0             ZERO MATCHES ALL                             
         BE    FH70                                                             
         CLI   IFSYS,C'*'          SO DOES *                                    
         BE    FH70                                                             
         CLC   IFSYS,PQSYS         TEST SYS                                     
         BNE   FHNO                                                             
*                                                                               
FH70     OC    IFPRG,IFPRG         ZERO MATCHES ALL                             
         BE    FH80                                                             
         CLC   IFPRG,=C'**'        SO DOES *                                    
         BE    FH80                                                             
         CLC   IFPRG,PQPRG         TEST PROG                                    
         BNE   FHNO                                                             
*                                                                               
FH80     OC    IFSUB,IFSUB         ZERO MATCHES ALL                             
         BE    FH90                                                             
         CLC   IFSUB,=C'**'        SO DOES *                                    
         BE    FH90                                                             
         CLC   IFSUB,PQPRGSUB      TEST SUB PROG                                
         BNE   FHNO                                                             
*                                                                               
FH90     EQU   *                                                                
*                                                                               
FHYES    B     YES                                                              
*                                                                               
FHNO     B     NO                                                               
         EJECT                                                                  
*        INVALID FILTER OBJECT                                                  
ERRFILT  MVC   MDACTION,=Y(ER02FILT)                                            
         B     EXIT                                                             
*        INVALID OFFICE CODE                                                    
ERROFF   MVC   MDACTION,=Y(ER02OFF)                                             
         B     EXIT                                                             
*        OFFICE CODE DOESN'T MATCH SIGN ON                                      
ERRMAT   MVC   MDACTION,=Y(ER02MAT)                                             
         B     EXIT                                                             
*                                                                               
EXIT     L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
* DMPRTQD                                                                       
       ++INCLUDE DMPRTQD                                                        
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
NUMSKIP  DS    F                   NUMBER OF INDEXES TO SKIP THIS FRM           
INQFILT  DS    CL(IFLENQ)          INQUIRY FILTER BLOCK                         
ATTRPOS  DS    X                   REPORT ATTRIBUTES POSITIVE FILTER            
ATTRNEG  DS    X                   REPORT ATTRIBUTES NEGITIVE FILTER            
STATPOS  DS    X                   REPORT STATUS POSITIVE FILTER                
STATNEG  DS    X                   REPORT STATUS NEGITIVE FILTER                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTMAD02   07/20/20'                                      
         END                                                                    
