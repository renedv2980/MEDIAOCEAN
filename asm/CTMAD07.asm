*          DATA SET CTMAD07    AT LEVEL 007 AS OF 05/01/02                      
*PHASE TA0C07A                                                                  
         TITLE 'TA0C07 - $MAD UNUSED PHASE'                                     
TA0C07   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C07,RA                                                      
*                                                                               
*        INITIALIZE OVERLAY WIDE REGISTERS                                      
*                                                                               
         L     RC,0(R1)            RC = A(CONTROLLER'S STORAGE)                 
         USING CONTROLD,RC                                                      
         L     R9,AOVER            R9 = A(OVERLAY'S STORAGE)                    
         USING OVERD,R9                                                         
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
         BNE   EXIT                                                             
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
*                                                                               
         CLI   LENTYPE,0           MUST BE TYPE FILTER OBJECT                   
         BNE   ERRFILT                                                          
         CLC   ATYPE,=A(ITINQFLT)                                               
         BNE   ERRFILT                                                          
*                                                                               
         CLC   DATALEN,=A(IFLENQ)  MUST BE CORRECT LENGTH                       
         BNE   ERRFILT                                                          
*                                                                               
         L     R4,ADATA            SAVE FILTER IN FILTER BLOCK                  
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
*                                  VALIDATE REP ATTR POSSITIVE FILTER           
         GOTO1 CHRTOHEX,DMCB,IFATTRP,2                                          
         BNE   ERRFILT                                                          
         MVC   ATTRPOS,0(R1)                                                    
*                                  VALIDATE REP ATTR NEGITIVE FILTER            
         GOTO1 CHRTOHEX,DMCB,IFATTRN,2                                          
         BNE   ERRFILT                                                          
         MVC   ATTRNEG,0(R1)                                                    
*                                  VALIDATE REP STAT POSSITIVE FILTER           
         GOTO1 CHRTOHEX,DMCB,IFSTATP,2                                          
         BNE   ERRFILT                                                          
         MVC   STATPOS,0(R1)                                                    
*                                  VALIDATE REP STAT NEGITIVE FILTER            
         GOTO1 CHRTOHEX,DMCB,IFSTATN,2                                          
         BNE   ERRFILT                                                          
         MVC   STATNEG,0(R1)                                                    
*                                                                               
VFX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE FILLS THE FRAME WITH REPORT INQUIRY OBJECTS.  IT SKIPS           
* THE FIRST 'NUMSKIP' NUMBER OF REPORTS TO GET TO THE FIRST REPORT.             
* UPON EXIT 'NUMSKIP' WILL HOLD THE NUMBER OF REPORTS TO SKIP TO GET TO         
* THE NEXT FRAME.                                                               
*                                                                               
FILLFRM  NTR1                                                                   
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
         LA    R2,ITDETINQ         R2 = ITEM TYPE                               
         LA    R3,IQDETLEN         R3 = DATA LENGTH                             
         CLC   PCACTION,=Y(ACDETINQ)                                            
         BE    FF40                                                             
         LA    R2,ITSUMINQ                                                      
         LA    R3,IQSUMLEN                                                      
*                                  PUT INQUIRY ITEM                             
FF40     GOTO1 PUTITEM,DMCB,(R2),(R3),INQUIRY                                   
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME THEN DONE                    
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
         ICM   RF,15,ATTRPOS       IF ATTR POSSITIVE FILTER IS NOT ZERO         
         BZ    FH10                                                             
         EX    RF,*+8              THEN SOME BIT SET IN POSS FILTER             
         B     *+8                     MUST BE SET IN INQUIRY                   
         TM    REPATTR,0                                                        
         BZ    FHNO                                                             
*                                                                               
FH10     ICM   RF,15,ATTRNEG       IF ATTR NEGITIVE FILTER IS NOT ZERO          
         BZ    FH20                                                             
         EX    RF,*+8              THEN SOME BIT SET IN NEG FILTER              
         B     *+8                     MUST BE CLEAR IN INQUIRY                 
         TM    REPATTR,0                                                        
         BO    FHNO                                                             
*                                                                               
FH20     ICM   RF,15,STATPOS       IF STAT POSSITIVE FILTER IS NOT ZERO         
         BZ    FH30                                                             
         EX    RF,*+8              THEN SOME BIT SET IN POSS FILTER             
         B     *+8                     MUST BE SET IN INQUIRY                   
         TM    REPSTAT,0                                                        
         BZ    FHNO                                                             
*                                                                               
FH30     ICM   RF,15,STATNEG       IF STAT NEGITIVE FILTER IS NOT ZERO          
         BZ    FH40                                                             
         EX    RF,*+8              THEN SOME BIT SET IN NEG FILTER              
         B     *+8                     MUST BE CLEAR IN INQUIRY                 
         TM    REPSTAT,0                                                        
         BO    FHNO                                                             
*                                                                               
FH40     CLI   IFFORMAT,C'*'       IF FORMAT FILTER = '*' THEN SKIP             
         BE    FH50                                                             
         CLC   IFFORMAT,IQFORMAT   ELSE INQUIRY MUST MATCH FILTER               
         BNE   FHNO                                                             
*                                  IF ACTION IS DETAILED INQUIRY                
FH50     CLC   PCACTION,=Y(ACDETINQ)                                            
         BNE   FH60                                                             
         CLC   IFCREATE,IQCREATE   THEN IF FILT CREATE DATE/TIME AFTER          
         BH    FHNO                    INQ DATE/TIME THEN RETURN 'NO'           
*                                                                               
FH60     DS    0H                                                               
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
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
NUMSKIP  DS    F                   NUMBER OF INDEXES TO SKIP THIS FRM           
INQFILT  DS    CL(IFLENQ)          INQUIRY FILTER BLOCK                         
ATTRPOS  DS    XL4                 REPORT ATTRIBUTES POSSITIVE FILTER           
ATTRNEG  DS    XL4                 REPORT ATTRIBUTES NEGITIVE FILTER            
STATPOS  DS    XL4                 REPORT STATUS POSSITIVE FILTER               
STATNEG  DS    XL4                 REPORT STATUS NEGITIVE FILTER                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CTMAD07   05/01/02'                                      
         END                                                                    
